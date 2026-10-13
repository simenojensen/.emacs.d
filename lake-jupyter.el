;;; lake-jupyter.el --- Local Python buffers, cluster Jupyter kernels -*- lexical-binding: t; -*-

;; Load AFTER your existing Python/Jupyter configuration.  Uses your already
;; installed emacs-jupyter package and the existing JupyterHub server.
;; Start your server at https://jupyter.data.safeseaway.com first.

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defvar my/lake-context "aks-mo-data-prod-weu")
(defvar my/lake-namespace "notebooks")
(defvar my/lake-email nil
  "Notebook owner email, or nil to use the signed-in Azure account.")
(defvar my/lake-kernel "python3"
  "Name of the Python kernelspec installed in the notebook pod.")
(defvar my/lake-run-directory "/home/jovyan/.emacs-runs"
  "Directory in the notebook pod for uploaded local Python projects.")
(defvar-local my/lake-project-root nil
  "Optional local project root; otherwise use the enclosing Git checkout.
Outside Git, only Python files beside the current script are uploaded.")
(defconst my/lake--picker-name "lake-kubernetes")

(defvar my/lake--forward nil)
(defvar my/lake--client nil)
(defvar my/lake--pod-name nil)

(defun my/lake--local-output (program &rest args)
  "Run PROGRAM locally and return its stdout without logging credentials."
  (apply #'my/lake--local-input-output nil program args))

(defun my/lake--local-input-output (input-file program &rest args)
  "Run PROGRAM locally, with INPUT-FILE as stdin, and return stdout."
  (let ((default-directory temporary-file-directory)
        (stderr-file (make-temp-file "lake-stderr-")))
    (unwind-protect
        (with-temp-buffer
          (let ((status (apply #'call-process program input-file (list t stderr-file) nil args)))
            (unless (equal status 0)
              ;; Only stderr is reported: stdout may contain a server token.
              (let ((diagnostic (with-temp-buffer
                                  (insert-file-contents stderr-file)
                                  (string-trim (buffer-string)))))
                (user-error "%s failed (%s): %s" program status
                            (if (string-empty-p diagnostic)
                                "No error diagnostic was returned"
                              (truncate-string-to-width diagnostic 4000 nil nil "...")))))
            (string-trim (buffer-string))))
      (delete-file stderr-file))))

(defun my/lake--kubectl (&rest args)
  (apply #'my/lake--local-output "kubectl"
         "--context" my/lake-context "--namespace" my/lake-namespace args))

(defun my/lake--pod ()
  "Find the running notebook pod owned by `my/lake-email'."
  (let* ((email (or my/lake-email
                    (my/lake--local-output
                     "az" "account" "show" "--query" "user.name" "-o" "tsv")))
         (data (json-parse-string (my/lake--kubectl "get" "pods" "-o" "json")
                                  :object-type 'alist :array-type 'list))
         (pod
          (cl-find-if
           (lambda (item)
             (let* ((metadata (alist-get 'metadata item))
                    (annotations (alist-get 'annotations metadata))
                    (owner (alist-get 'hub.jupyter.org/username annotations "")))
               (and (equal (downcase owner) (downcase email))
                    (not (alist-get 'deletionTimestamp metadata))
                    (equal (alist-get 'phase (alist-get 'status item)) "Running"))))
           (alist-get 'items data))))
    (unless pod
      (user-error "No running notebook pod for %s; start your Jupyter server first" email))
    (alist-get 'name (alist-get 'metadata pod))))

(defun my/lake--server-info (pod)
  "Read the server's port, base URL and token directly from POD.
The token is kept in memory, not written to configuration or a log file."
  (json-parse-string
   (my/lake--kubectl
    "exec" pod "-c" "notebook" "--" "python3" "-c"
    (concat
     "import json, os; from urllib.parse import urlparse; "
     "u=urlparse(os.environ['JUPYTERHUB_SERVICE_URL']); "
     "print(json.dumps({'port':u.port or 8888, "
     "'base':os.environ['JUPYTERHUB_SERVICE_PREFIX'], "
     "'token':os.environ['JUPYTERHUB_API_TOKEN']}))"))
   :object-type 'alist))

(defun my/lake--start-forward (pod remote-port)
  "Forward a randomly allocated localhost port to POD's REMOTE-PORT."
  (let* ((default-directory temporary-file-directory)
         (buffer (get-buffer-create "*lake-port-forward*"))
         (deadline (+ (float-time) 20))
         local-port)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)) (erase-buffer)))
    (setq my/lake--forward
          (make-process
           :name "lake-port-forward" :buffer buffer :noquery t
           :command (list "kubectl" "--context" my/lake-context
                          "--namespace" my/lake-namespace
                          "port-forward" "--address" "127.0.0.1"
                          (concat "pod/" pod) (format ":%s" remote-port))))
    (while (and (process-live-p my/lake--forward)
                (not local-port) (< (float-time) deadline))
      (accept-process-output my/lake--forward 0.1)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "Forwarding from 127\\.0\\.0\\.1:\\([0-9]+\\)" nil t)
            (setq local-port (string-to-number (match-string 1)))))))
    (unless local-port
      (user-error "Port forwarding failed; see *lake-port-forward*"))
    local-port))

(defun my/lake--connected-p ()
  (and my/lake--client
       (process-live-p my/lake--forward)
       (condition-case nil
           (let ((jupyter-current-client my/lake--client))
             (jupyter-repl-connected-p))
         (error nil))))

(defun my/lake--stop-forward ()
  "Stop only the local tunnel; do not shut down the user's Jupyter server."
  (when (process-live-p my/lake--forward)
    (delete-process my/lake--forward))
  (setq my/lake--forward nil my/lake--client nil my/lake--pod-name nil))

(cl-defun my/lake-jupyter (&optional repl-name (associate-buffer t)
                                    client-class (display t))
  "Open a cluster REPL and associate this local Python buffer with it.
Reuse this Emacs session's Lake kernel while its connection is alive."
  (interactive)
  (require 'jupyter-server)
  (unless (my/lake--connected-p)
    (my/lake--stop-forward)
    (message "Connecting to your Lake notebook server...")
    (condition-case err
        (let* ((pod (my/lake--pod))
               (info (my/lake--server-info pod))
               (port (my/lake--start-forward pod (alist-get 'port info)))
               (server (jupyter-server
                        :url (string-remove-suffix
                              "/" (format "http://127.0.0.1:%s%s"
                                          port (alist-get 'base info)))
                        :auth (list (cons "Authorization"
                                          (concat "token " (alist-get 'token info)))))))
          (setq my/lake--client
                (jupyter-run-server-repl server my/lake-kernel
                                         (or repl-name "Lake") nil client-class nil)
                my/lake--pod-name pod))
      ((error quit)
       (my/lake--stop-forward)
       (signal (car err) (cdr err)))))
  ;; Some emacs-jupyter releases require an exact major-mode match.
  ;; Enabling interaction mode explicitly also supports python-ts-mode.
  (when (and associate-buffer (derived-mode-p 'python-mode 'python-ts-mode))
    (setq-local jupyter-current-client my/lake--client)
    (jupyter-repl-interaction-mode 1))
  (when display
    (let ((jupyter-current-client my/lake--client))
      (jupyter-repl-pop-to-buffer)))
  my/lake--client)

(defun my/lake--select-kernelspec (&optional refresh)
  "Use the normal Jupyter picker with an additional Lake entry.
Keep local kernelspec discovery and REFRESH behavior unchanged.  Cluster
discovery is deferred until the Lake entry is actually selected."
  (let ((specs (jupyter-kernelspecs default-directory refresh)))
    (unless (file-remote-p default-directory)
      (setq specs
            (append specs
                    (list (make-jupyter-kernelspec
                           :name my/lake--picker-name
                           :plist '(:display_name "Lake (Kubernetes)"
                                    :language "python"))))))
    (jupyter-completing-read-kernelspec specs)))

(defun my/lake--run-repl (original kernel-name &optional repl-name
                                 associate-buffer client-class display)
  "Extend `jupyter-run-repl' with a Lake choice in its interactive picker."
  (interactive
   (list (jupyter-kernelspec-name
          (my/lake--select-kernelspec current-prefix-arg))
         (when current-prefix-arg (read-string "REPL Name: "))
         t nil t))
  (if (equal kernel-name my/lake--picker-name)
      (my/lake-jupyter repl-name associate-buffer client-class display)
    (funcall original kernel-name repl-name associate-buffer client-class display)))

(defconst my/lake--upload-python
  (mapconcat
   #'identity
   '("import base64, json, os, sys, tempfile"
     "from pathlib import Path"
     "root = Path(sys.argv[1]).expanduser().resolve()"
     "root.mkdir(parents=True, exist_ok=True, mode=0o700)"
     "payload = json.load(sys.stdin)"
     "manifest = root / '.lake-files.json'"
     "previous = json.loads(manifest.read_text()) if manifest.exists() else []"
     "def target(name):"
     "    p = root / name"
     "    if Path(name).is_absolute() or not p.resolve().is_relative_to(root):"
     "        raise ValueError('Invalid project path: ' + name)"
     "    return p"
     "def clear_cache(p):"
     "    for cache in (p.parent / '__pycache__').glob(p.stem + '.*.pyc'):"
     "        cache.unlink()"
     "def write(p, data):"
     "    if p.exists() and p.read_bytes() == data:"
     "        return"
     "    p.parent.mkdir(parents=True, exist_ok=True, mode=0o700)"
     "    fd, temporary = tempfile.mkstemp(dir=p.parent)"
     "    try:"
     "        with os.fdopen(fd, 'wb') as stream:"
     "            stream.write(data)"
     "        os.replace(temporary, p)"
     "    finally:"
     "        if os.path.exists(temporary):"
     "            os.unlink(temporary)"
     "    clear_cache(p)"
     "files = payload['files']"
     "for name, data in files.items():"
     "    write(target(name), base64.b64decode(data, validate=True))"
     "for name in set(previous) - set(files):"
     "    p = target(name)"
     "    p.unlink(missing_ok=True)"
     "    clear_cache(p)"
     "write(root / '.lake-prepare.py', payload['prepare'].encode('utf-8'))"
     "write(manifest, json.dumps(sorted(files)).encode('utf-8'))")
   "\n")
  "Pod-side uploader; preserves unchanged mtimes and removes stale sources.")

(defconst my/lake--prepare-python
  (mapconcat
   #'identity
   '("import importlib, json, os, sys"
     "from pathlib import Path"
     "from IPython import get_ipython"
     "root = Path(__file__).resolve().parent"
     "ip = get_ipython()"
     "files = json.loads((root / '.lake-files.json').read_text())"
     "names = {Path(f).stem for f in files if '/' not in f}"
     "names.update(f.split('/')[0] for f in files if '/' in f and"
     "             (root / f.split('/')[0] / '__init__.py').is_file())"
     "# %run also makes imports beside a deeply nested script available."
     "script_dir = Path(__lake_script__).parent"
     "names.update(p.stem for p in script_dir.glob('*.py'))"
     "old_root = ip.user_ns.get('_lake_project_root')"
     "old_names = ip.user_ns.get('_lake_project_names', set())"
     "switching = old_root != str(root)"
     "for name, module in list(sys.modules.items()):"
     "    if name in ('__main__', '__mp_main__'):"
     "        continue"
     "    source = getattr(module, '__file__', None)"
     "    from_old = source and old_root and Path(source).is_relative_to(old_root)"
     "    if name.split('.')[0] in (names | old_names) or from_old:"
     "        # Changing sys.path alone cannot replace an imported package."
     "        if switching or (source and Path(source).is_relative_to(root)"
     "                         and not Path(source).exists()):"
     "            sys.modules.pop(name, None)"
     "if old_root and old_root != str(root):"
     "    sys.path[:] = [p for p in sys.path if p != old_root]"
     "sys.path[:] = [p for p in sys.path if p != str(root)]"
     "sys.path.insert(0, str(root))"
     "os.chdir(root)"
     "importlib.invalidate_caches()"
     "ip.user_ns['_lake_project_root'] = str(root)"
     "ip.user_ns['_lake_project_names'] = names"
     "if 'IPython.extensions.autoreload' not in ip.extension_manager.loaded:"
     "    ip.run_line_magic('load_ext', 'autoreload')"
     "ip.run_line_magic('autoreload', '2')"
     "# Reload now as well: setup and %run are in the same execution request."
     "ip.run_line_magic('autoreload', '')"
     "if switching:"
     "    print('[Lake] local Python project: ' + str(root))")
   "\n")
  "Kernel-side setup; selects the uploaded project and refreshes imports.")

(defun my/lake--project-root (file)
  "Return the local project root containing FILE."
  (let ((root (file-name-as-directory
               (expand-file-name
                (or my/lake-project-root
                    (locate-dominating-file file ".git")
                    (file-name-directory file))))))
    (unless (file-in-directory-p file root)
      (user-error "Python file is outside my/lake-project-root: %s" file))
    root))

(defun my/lake--project-files (root file)
  "Return saved Python sources in ROOT, including new, non-ignored files."
  (let ((files
         (if (file-exists-p (expand-file-name ".git" root))
             (split-string
              (my/lake--local-output
               "git" "-C" root "ls-files" "-z" "--cached" "--others"
               "--exclude-standard" "--" "*.py")
              "\0" t)
           (if my/lake-project-root
               (mapcar (lambda (path) (file-relative-name path root))
                       (directory-files-recursively root "\\.py\\'" nil
                                                    (lambda (dir)
                                                      (not (member
                                                            (file-name-nondirectory dir)
                                                            '(".git" ".venv" "venv" "__pycache__"))))))
             (directory-files root nil "\\.py\\'")))))
    (cl-remove-if-not
     (lambda (name)
       (let ((path (expand-file-name name root)))
         (and (file-regular-p path) (file-in-directory-p path root))))
     (delete-dups (cons (file-relative-name file root) files)))))

(defun my/lake--upload-project (root file)
  "Upload ROOT's Python sources, returning FILE's corresponding pod path."
  (let* ((pod (or my/lake--pod-name
                  (setq my/lake--pod-name (my/lake--pod))))
         ;; A separate directory per checkout keeps relative imports intact.
         (directory (substring (secure-hash 'sha256 root) 0 16))
         (remote-root (concat (directory-file-name my/lake-run-directory)
                              "/" directory))
         (payload-file (make-temp-file "lake-project-"))
         (files (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (dolist (name (my/lake--project-files root file))
            (puthash name
                     (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert-file-contents-literally (expand-file-name name root))
                       (base64-encode-string (buffer-string) t))
                     files))
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file payload-file
              (insert (json-serialize
                       (list :files files :prepare my/lake--prepare-python)))))
          (message "Uploading %d Python files to Lake..." (hash-table-count files))
          (my/lake--local-input-output
           payload-file "kubectl" "--context" my/lake-context
           "--namespace" my/lake-namespace "exec" "-i" pod "-c" "notebook"
           "--" "python3" "-c" my/lake--upload-python remote-root)
          (concat remote-root "/" (file-relative-name file root)))
      (delete-file payload-file))))

(defun my/lake-execute-buffer ()
  "Save project buffers with Projectile, upload Python sources, and run this file.
The -i option retains Lake's injected helpers and the kernel's variables.
Project imports use the uploaded checkout, with autoreload enabled.
Only setup and %run commands are sent to the REPL.  Upload happens on
every invocation so execution uses the current saved edits."
  (interactive)
  (unless (and (my/lake--connected-p)
               (eq jupyter-current-client my/lake--client))
    (user-error "Connect this buffer to Lake with C-c C-p first"))
  (unless (and buffer-file-name (not (file-remote-p buffer-file-name)))
    (user-error "C-c C-l requires a local Python file; save this buffer first"))
  (let ((root (my/lake--project-root buffer-file-name)))
    (save-restriction
      (widen)
      (save-buffer))
    (if (and (require 'projectile nil t) (projectile-project-p))
        (projectile-save-project-buffers)
      ;; Keep standalone scripts usable outside a Projectile project.
      (save-some-buffers
       t (lambda ()
           (and buffer-file-name
                (string-suffix-p ".py" buffer-file-name)
                (file-in-directory-p buffer-file-name root)))))
    (let* ((remote-file (my/lake--upload-project root buffer-file-name))
           (remote-root (substring remote-file 0
                                   (- (length (file-relative-name buffer-file-name root)))))
           (setup-file (concat remote-root ".lake-prepare.py")))
      (jupyter-eval-string
       (format "exec(compile(open(%s).read(), %s, 'exec'), {'__file__': %s, '__lake_script__': %s})\n%%run -i %s"
               (json-serialize setup-file) (json-serialize setup-file)
               (json-serialize setup-file) (json-serialize remote-file)
               (json-serialize remote-file))))))

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-p") #'jupyter-run-repl)
  (when (boundp 'python-ts-mode-map)
    (define-key python-ts-mode-map (kbd "C-c C-p") #'jupyter-run-repl)))

(with-eval-after-load 'jupyter-repl
  ;; Minor-mode bindings override the bindings in python-mode-map.
  (advice-add 'jupyter-run-repl :around #'my/lake--run-repl)
  (define-key jupyter-repl-interaction-mode-map (kbd "C-c C-p") #'jupyter-run-repl)
  (define-key jupyter-repl-interaction-mode-map (kbd "C-c C-l")
    (lambda ()
      (interactive)
      (if (and my/lake--client
               (eq jupyter-current-client my/lake--client))
          (my/lake-execute-buffer)
        (call-interactively #'jupyter-load-file)))))

(defun my/lake--unique-websocket-headers (options)
  "Remove repeated identical headers from WebSocket OPTIONS.
Some emacs-jupyter releases add Authorization once in the kernel client
and again in the REST API.  JupyterHub rejects the resulting doubled token."
  (if (plist-member options :custom-header-alist)
      (plist-put options :custom-header-alist
                 (cl-remove-duplicates
                  (plist-get options :custom-header-alist) :test #'equal))
    options))

(with-eval-after-load 'jupyter-rest-api
  (advice-add 'jupyter-api-add-websocket-headers :filter-return
              #'my/lake--unique-websocket-headers))

(add-hook 'kill-emacs-hook #'my/lake--stop-forward)
(provide 'lake-jupyter)
;;; lake-jupyter.el ends here
