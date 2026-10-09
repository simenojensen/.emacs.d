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

(defvar my/lake--forward nil)
(defvar my/lake--client nil)

(defun my/lake--local-output (program &rest args)
  "Run PROGRAM locally and return its stdout without logging credentials."
  (let ((default-directory temporary-file-directory))
    (with-temp-buffer
      (unless (zerop (apply #'call-process program nil (list t nil) nil args))
        (user-error "%s failed; check cluster access and your running notebook server"
                    program))
      (string-trim (buffer-string)))))

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
    "exec" pod "-c" "notebook" "--" "python" "-c"
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
  (setq my/lake--forward nil my/lake--client nil))

(defun my/lake-jupyter ()
  "Open a cluster REPL and associate this local Python buffer with it.
Reuse this Emacs session's Lake kernel while its connection is alive."
  (interactive)
  (require 'jupyter-server)
  (unless (derived-mode-p 'python-mode 'python-ts-mode)
    (user-error "Run this command from a Python buffer"))
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
                (jupyter-run-server-repl server my/lake-kernel "Lake" nil nil nil)))
      ((error quit)
       (my/lake--stop-forward)
       (signal (car err) (cdr err)))))
  ;; Some emacs-jupyter releases require an exact major-mode match.
  ;; Enabling interaction mode explicitly also supports python-ts-mode.
  (setq-local jupyter-current-client my/lake--client)
  (jupyter-repl-interaction-mode 1)
  (jupyter-repl-pop-to-buffer))

(defun my/lake-execute-buffer ()
  "Send the entire current local buffer to its connected Lake kernel.
Includes unsaved edits.  No Mac file path is sent for the kernel to open."
  (interactive)
  (unless (and (my/lake--connected-p)
               (eq jupyter-current-client my/lake--client))
    (user-error "Connect this buffer to Lake with C-c C-p first"))
  (save-restriction
    (widen)
    (jupyter-eval-buffer (current-buffer))))

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-p") #'my/lake-jupyter)
  (when (boundp 'python-ts-mode-map)
    (define-key python-ts-mode-map (kbd "C-c C-p") #'my/lake-jupyter)))

(with-eval-after-load 'jupyter-repl
  ;; Minor-mode bindings override the bindings in python-mode-map.
  (define-key jupyter-repl-interaction-mode-map (kbd "C-c C-p") #'my/lake-jupyter)
  (define-key jupyter-repl-interaction-mode-map (kbd "C-c C-l")
    (lambda ()
      (interactive)
      (if (and my/lake--client
               (eq jupyter-current-client my/lake--client))
          (my/lake-execute-buffer)
        (call-interactively #'jupyter-load-file)))))

(add-hook 'kill-emacs-hook #'my/lake--stop-forward)
(provide 'lake-jupyter)
;;; lake-jupyter.el ends here
