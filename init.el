;;; init.el --- -*- lexical-binding: t -*-

(defvar file-name-handler-alist-original file-name-handler-alist
  "Save the original `file-name-handler-alist' for restoration after startup.")

(setq file-name-handler-alist nil) ; Disable file-name handlers temporarily

(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(defvar better-gc-cons-threshold (* 100 1024 1024) ; 100 MB
  "The default value to use for `gc-cons-threshold'.
   If you experience freezing, decrease this. If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold
                  gc-cons-percentage 0.1) ; Reset to a reasonable default
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Setup straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install and configure use-package
(straight-use-package 'use-package)

;; Ensure that packages are installed automatically if not present
(setq straight-use-package-by-default t)

(use-package diminish)

(setq-default user-full-name "Simen Omholt-Jensen")
(setq-default user-mail-address "simen@omholt-jensen.com")

(setq-default read-process-output-max (* 2 1024 1024))                                  ;; Increase the amount of data which Emacs reads from the process

(global-display-line-numbers-mode)                                                      ;; Display line numbers
(dolist (mode '(org-mode-hook                                                           ;; Disable line numbers for some modes
                term-mode-hook
                vterm-mode-hook
                jupyter-repl-mode-hook
                eshell-mode-hook
                pdf-view-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
(setq-default column-number-mode t)                                                     ;; Display column numbers
(setq-default fill-column 80)                                                           ;; Set fill column to 80 chars by default

(setq-default inhibit-startup-screen t)                                                 ;; Don't show the startup message
(setq inhibit-startup-echo-area-message t)                                              ;; Don't show the startup echo message
(setq-default initial-scratch-message nil)                                              ;; Set initial scratch message to nil

(set-default 'truncate-lines t)                                                         ;; default truncate lines
(setq debug-on-error nil)                                                               ;; Receive more information errors

(setq custom-file "~/.emacs.d/custom.el")                                               ;; Set the path for custom-file
(unless (file-exists-p custom-file)                                                     ;; Create the custom file if it does not exist
  (with-temp-buffer (write-file custom-file)))                                          ;; Load custom-file if it exists
(ignore-errors (load custom-file))

(setq-default indent-tabs-mode nil)                                                     ;; Don't use hard tabs
(setq echo-keystrokes 0.1)                                                              ;; Echo keystrokes fast
(fset 'yes-or-no-p 'y-or-n-p)                                                           ;; y-or-n instead of yes-or-no
(add-hook 'before-save-hook 'delete-trailing-whitespace)                                ;; Delete trailing whitespace on save
(setq require-final-newline t)                                                          ;; Add a newline at end of file on save
(global-auto-revert-mode t)                                                             ;; Automatically update buffers if a file content has changed on disk
(save-place-mode t)                                                                     ;; Save position of the point in file
(global-hl-line-mode t)                                                                 ;; Highlight the line with the point
(setq large-file-warning-threshold 100000000)                                           ;; Warn when opening file larger than 100 MB
(desktop-save-mode 1)                                                                   ;; save desktop
(setq history-delete-duplicates t)                                                      ;; delete duplicate history
(setq revert-without-query '(".*"))                                                     ;; do not ask when reverting buffer
(setq-default cursor-type '(bar . 4))                                                   ;; use bar for cursor
(bind-key "<escape>" 'keyboard-escape-quit)                                             ;; Cancel on escape

;; Function to ensure a directory exists
(defun ensure-directory-exists (dir)
  "Ensure that the directory DIR exists."
  (unless (file-exists-p dir)
    (make-directory dir t)))

;; Define a centralized directory for all Emacs-generated files
(defvar emacs-temp-files-dir (expand-file-name "temp-files/" user-emacs-directory)
  "Directory for storing Emacs-generated files to reduce clutter.")

;; Ensure the temp-files directory exists
(unless (file-exists-p emacs-temp-files-dir)
  (make-directory emacs-temp-files-dir t))

;; Ensure necessary directories exist
(dolist (dir '("backups" "auto-saves" "auto-save-list" "locks" "undo-tree-history"))
  (ensure-directory-exists (expand-file-name dir emacs-temp-files-dir)))

;; Backup files
(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" emacs-temp-files-dir))))

;; Lock files
(setq lock-file-name-transforms
      `((".*" ,(expand-file-name "locks/" emacs-temp-files-dir) t)))

;; auto-save files
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-saves/" emacs-temp-files-dir) t)))
(setq auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" emacs-temp-files-dir))

;; Undo-tree configuration using use-package
(use-package undo-tree
  :diminish
  :init
  (setq undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo-tree-history/" emacs-temp-files-dir))))
  (setq undo-tree-auto-save-history t)
  :config
  (global-undo-tree-mode 1))

;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)

(menu-bar-mode -1)                         ;; Disable menu bar
(tool-bar-mode -1)                         ;; Disable tool bar
(scroll-bar-mode -1)                       ;; Disable scroll bar
(blink-cursor-mode -1)                     ;; Disable blinking cursor
(setq-default ring-bell-function 'ignore)  ;; Disable bell function

(setq-default confirm-kill-emacs nil)        ;; Do not confirm when killing Emacs
(setq-default confirm-kill-processes nil)    ;; do not confirm when killing processes before killing Emacs

(cond ((eq system-type 'darwin)
       (setq mac-command-modifier 'meta)
       (setq mac-right-command-modifier 'super)
       (setq mac-option-modifier 'alt)
       (setq mac-right-option-modifier 'hyper)
       (bind-key "M-=" 'text-scale-increase)
       (bind-key "M--" 'text-scale-decrease)
       (bind-key "M-`" 'other-frame)
       (use-package exec-path-from-shell
         :config
         (setq shell-file-name "/opt/homebrew/bin/zsh") ;; Let emacs know which shell to use.
         (setq exec-path-from-shell-variables  '("PATH" "R_HOME" "MANPATH" "VIRTUAL_ENV" "PKG_CONFIG_PATH" "GOPATH"))
         (setenv "PYTHONPATH" "/Applications/QGIS.app/Contents/Resources/python:/Applications/QGIS.app/Contents/Resources/python/plugins")
         (exec-path-from-shell-initialize))
       )
      ((eq system-type 'windows-nt)
       
       )
      ((eq system-type 'gnu/linux)
       
       ))

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5)
  (setq which-key-frame-max-height 40)
  (which-key-mode))

(bind-key "s-<left>" 'shrink-window-horizontally)
(bind-key "s-<right>" 'enlarge-window-horizontally)
(bind-key "s-<down>" 'shrink-window)
(bind-key "s-<up>" 'enlarge-window)

(bind-key "C-x C-l" 'toggle-truncate-lines)

(bind-key "M-p" 'backward-paragraph)
(bind-key "M-n" 'forward-paragraph)
(bind-key "M-g" 'goto-line)

(bind-key "C-x b" 'ibuffer-other-window)

(use-package crux
  :bind
  ("C-a" . crux-move-beginning-of-line))

(use-package amx)

  (use-package counsel
    :diminish
    :config (counsel-mode 1))

  (use-package swiper)

  (use-package ivy
    :diminish
    :init
    (ivy-mode 1)
    :bind
    (("C-x C-f" . counsel-find-file)
     ("C-x f" . counsel-fzf)
     ("C-x C-b" . counsel-switch-buffer)
     ("C-h f" . counsel-describe-function)
     ("C-h v" . counsel-describe-variable)
     ("C-h l" . counsel-find-library)
     ("C-h i" . counsel-info-lookup-symbol)
     ("C-h u" . counsel-unicode-char)
     ("C-c k" . counsel-rg)
     ("C-x l" . counsel-locate)
     ("M-x" . counsel-M-x)
     ("M-v" . counsel-yank-pop)
     ("C-s" . swiper-isearch)
     :map ivy-minibuffer-map
     ("A-<tab>" . ivy-mark) ;; Mark multiple candidates
     ("C-<return>" . ivy-call) ;; perform call
     )
    :config
    (ivy-mode 1)
    (setq ivy-height 20)
    (setq ivy-initial-inputs-alist nil)
    (setq ivy-display-style 'fancy)
    (setq ivy-use-selectable-prompt t)
    (setq counsel-switch-buffer-preview-virtual-buffers nil)
    ;; sort counsel-rg results
    (setq ivy-sort-functions-alist
        '((counsel-rg . ivy-sort-file-function-default)
          (t . nil)))
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) "))

(use-package all-the-icons-ivy-rich
  :init
  (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package dired
  :straight nil
  :bind
  (("C-x C-j" . dired-jump)
   ("C-x j" . dired-jump-other-window))
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  ;; Auto refresh Dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
  ;; Quickly copy/move file in Dired
  (setq dired-dwim-target t)
  ;; Move files to trash when deleting
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash")
  ;; Load the newest version of a file
  (setq load-prefer-newer t)
  ;; Detect external file changes and auto refresh file
  (setq auto-revert-use-notify nil)
  (setq auto-revert-interval 3) ; Auto revert every 3 sec
  ;; sort directory first
  (setq insert-directory-program "/opt/homebrew/bin/gls"
        dired-use-ls-dired t)
  (setq dired-listing-switches "-laXGh --group-directories-first")
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil)
  :hook
  (dired-mode . (lambda ()
                  (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file)
                  (local-set-key (kbd "RET") #'dired-find-alternate-file)
                  (local-set-key (kbd "^")
                                 (lambda () (interactive) (find-alternate-file ".."))))))

(use-package all-the-icons-dired
  :diminish
  :custom-face
  (all-the-icons-dired-dir-face ((t (:foreground nil))))
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package ace-window
  :bind
  ("C-x C-o" . ace-window)
  ("C-x o" . ace-window)
  :init
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit fixed-pitch :height 4.0 :foreground "firebrick3"))))))

(winner-mode 1)

(use-package vterm
  ;; add functionality for counsel-yank-pop
  :after counsel
  :init
  ;; Counsel-yank-pop
  (defun vterm-counsel-yank-pop-action (orig-fun &rest args)
    (if (equal major-mode 'vterm-mode)
        (let ((inhibit-read-only t)
              (yank-undo-function (lambda (_start _end) (vterm-undo))))
          (cl-letf (((symbol-function 'insert-for-yank)
                     (lambda (str) (vterm-send-string str t))))
            (apply orig-fun args)))
      (apply orig-fun args)))

  (advice-add 'counsel-yank-pop-action :around #'vterm-counsel-yank-pop-action)
  (setq vterm-max-scrollback 10000)
  (setq vterm-always-compile-module t)
  )
;; (use-package multi-vterm)

(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package projectile
  :diminish
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map))
  :config
  (setq projectile-completion-system 'ivy))

(use-package company
  :diminish
  :init
  (global-company-mode)
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("<TAB>" . company-complete-common))
  (:map company-search-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("<TAB>" . company-complete-common))
  :config
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay
        (lambda () (if (company-in-string-or-comment) nil 0.1)))
  (setq company-tooltip-idle-delay 0.2)
  (setq company-echo-delay nil) ;; after some time the definition is shown in the echo area
  (setq company-tooltip-align-annotations t) ;; right-align description strings
  ;; backends
  (setq company-files-exclusions '(".git/" ".DS_Store"))
  )

(use-package company-box
  :diminish
  :hook
  (company-mode . company-box-mode)
  :bind
  (:map company-active-map
        ("C-h" . company-box-doc-manually))
  (:map company-search-map
        ("C-h" . company-box-doc-manually))
  :config
  (setq company-box-doc-enable nil)
  )

(use-package flycheck
  :diminish
  :hook
  (prog-mode . flycheck-mode))

;; :config
;; (setq flycheck-checker-error-threshold 1000)
;; (setq-default flycheck-c/c++-clang-executable "/usr/bin/clangd")
;; (setq-default flycheck-clang-standard-library "libc++")
;; (setq-default flycheck-clang-language-standard "c++20")
;; (setq-default flycheck-cppcheck-standards '("c++20"))
;;; (setq-default flycheck-clang-args "-std=c++20")
;; )

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (use-package yasnippet-snippets)
  (yas-global-mode t))

(use-package evil-nerd-commenter
  :bind
  ("C-;" . evilnc-comment-or-uncomment-lines))

(use-package lsp-mode
  :diminish
  :init
  (setq lsp-keymap-prefix "C-c l")
  :bind
  (:map lsp-mode-map
        ("M-;" . lsp-rename))
  :hook
  ((python-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :config
  ;; prefer ruff for linting and formatting
  (setq lsp-diagnostics-provider :none)
  (setq flycheck-checker 'python-ruff)
  )

(use-package lsp-ui
  :bind
  ;; lsp-ui-peek
  (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references)
         ("C-c d" . lsp-ui-doc-show)
         ("C-c i" . lsp-ui-doc-focus-frame)
         )
  :config
  ;; sideline
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  ;; doc
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-side 'right)
  (setq lsp-ui-doc-alignment 'frame)
  (setq lsp-ui-doc-max-width 160)
  (setq lsp-ui-doc-max-height 40)
  )
(use-package lsp-ivy)

(use-package format-all)

(use-package python
  :init
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-indent-offset 4)
  :hook
  (python-mode . (lambda()
                   (add-hook 'before-save-hook #'lsp-format-buffer t t)
                   (add-hook 'before-save-hook #'lsp-organize-imports t t))))

(use-package conda
  :hook
  (python-mode . (lambda () (conda-env-activate "py3")))
  :config
  (setq conda-env-home-directory "/opt/homebrew/Caskroom/miniconda/base/")
  (setq conda-anaconda-home "/opt/homebrew/Caskroom/miniconda/base/"))

(defun my/conda-python-path ()
  "Return the path to the Python executable for the current Conda environment."
  (when (and (boundp 'conda-env-current-path) conda-env-current-path)
    (let ((conda-python-path (concat conda-env-current-path "bin/python")))
      (when (file-executable-p conda-python-path)
        conda-python-path))))

(use-package lsp-pyright
  :after conda
  :config
  ;; (setq lsp-pyright-python-executable-cmd "/opt/homebrew/Caskroom/miniconda/base/envs/py3/bin/python")
  ;; (setq lsp-pyright-venv-path "/opt/homebrew/Caskroom/miniconda/base/envs/")
  ;; (setq lsp-pyright-venv-directory "/opt/homebrew/Caskroom/miniconda/base/envs/")
  (add-to-list 'lsp-pyright-python-search-functions #'my/conda-python-path))

(defun my/jupyter-load-file ()
  "Send current buffer to jupyter kernel by default"
  (interactive)
  (jupyter-load-file (buffer-file-name)))

(use-package jupyter
  :diminish
  :bind
  (:map python-mode-map
        ("C-c C-p" . jupyter-run-repl))
  :init
  (setq jupyter-repl-allow-RET-when-busy t)
  (setq jupyter-repl-echo-eval-p t)) ;; show plots

(use-package numpydoc
  :config
  (setq numpydoc-insert-examples-block nil)
  (setq numpydoc-insert-return-without-typehint t)
  )

(use-package ein)

(use-package go-mode
  :hook
  (go-mode . (lambda()
               (lsp-deferred)
               (add-hook 'before-save-hook #'lsp-format-buffer t t)
               (add-hook 'before-save-hook #'lsp-organize-imports t t))))

(use-package protobuf-mode
  :mode "\\.proto\\'")

(use-package csv-mode
  :mode "\\.[Cc][Ss][Vv]\\'")

;; Tangle on config file
(defun my/tangle-emacs-config ()
  "If the current file is this file, the code blocks are tangled"
  (when (equal (buffer-file-name) (expand-file-name "~/.emacs.d/my-literate-emacs-configuration.org"))
    (org-babel-tangle nil "~/.emacs.d/init.el")))

(use-package org
  :straight (:type built-in)
  :hook
  (after-save . my/tangle-emacs-config)
  (org-mode . (lambda ()
                ;; (flyspell-mode)
                (display-fill-column-indicator-mode)
                (auto-fill-mode)
                ))
  :init
  (use-package org-indent :straight (:type built-in))
  :config
  ;; -------------------- Org Agenda --------------------
  ;; Org settings
  (setq org-directory "~/Documents/Org") ;; Set default org directory
  (setq org-default-notes-file (concat org-directory "/tasks.org")) ;; Set default org capture file
  ;; Org agenda
  (setq org-todo-keywords
        '((sequence "TODO"  "|" "DONE" "CANCELED")))
  (setq org-agenda-files '("~/Documents/Org/"))
  (setq org-agenda-window-setup 'current-window)
  ;; org capture
  (setq org-capture-templates
        '(("a" "Assignment" entry
           (file+headline "~/Documents/Org/Academic.org" "Assignments")
           "* TODO %?\n")))
  ;; -------------------- Evaluation of Source Blocks --------------------
  ;; Do not confirm when evaluating code blocks
  (setq org-confirm-babel-evaluate nil)
  ;; Run/highlight code using babel in org-mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (js . t)
     (latex . t)
     (jupyter . t)
     (sql . t)
     (shell . t)
     (emacs-lisp . t)))
  ;; How to edit source code blocks: [plain, current-window, split-window-below, other-window, other-frame]
  (setq org-src-window-setup 'current-window)
  ;; Edit source code blocks menu
  (setq org-structure-template-alist
        '(("a" . "export ascii\n")
          ("c" . "center\n")
          ("C" . "comment\n")
          ("e" . "src emacs-lisp\n")
          ("E" . "export")
          ("h" . "export html\n")
          ("l" . "src latex\n")
          ("q" . "quote\n")
          ("p" . "src python\n")
          ("j" . "src jupyter-python\n")
          ("s" . "src sql")
          ("v" . "verse\n")))
  ;; -------------------- Export reveal --------------------
  (use-package htmlize)
  ;; -------------------- Various Behavior --------------------
  ;; Follow link when hitting return
  (setq org-return-follows-link t)
  ;; -------------------- Latex Exports --------------------
  ;; auctex
  (use-package tex
    :straight auctex)
  ;; Remove logfiles
  (setq org-latex-logfiles-extensions '(
                                        ;; Default settings
                                        "aux" "bcf" "blg" "fdb_latexmk" "fls" "figlist" "idx" "log" "nav" "out" "ptc" "run.xml" "snm" "toc" "vrb" "xdv"
                                        ;; Added settings
                                        "bbl" "lof" "lot" "tex" "glo" "ist" "glg" "gls" "acn" "acr" "alg" "loa"
                                        ))
  (setq org-latex-remove-logfiles t)
  ;; Set default figure position
  (setq org-latex-default-figure-position "H")
  ;; Set default caption position
  (setq org-latex-caption-above nil) ;; '("table" "image")
  ;; Set default export to async
  (setq org-export-in-background nil)
  ;; Remove default header exports
  (setq org-export-with-title t
        org-export-with-date t
        org-export-with-author t
        org-export-with-creator nil
        org-export-with-toc t
        )
  ;; add glossary and acronyms
  (add-to-list 'org-export-before-parsing-hook 'org-ref-acronyms-before-parsing)
  (add-to-list 'org-export-before-parsing-hook 'org-ref-glossary-before-parsing)
  ;; Latex compilation
  ;; (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "makeglossaries %b"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f"))
  ;; Latex classes
  (setq org-latex-classes
        '(("article"
           "
        \\documentclass[10pt]{article}
        % Setup
        \\usepackage[english]{babel}
        \\usepackage[utf8]{inputenc}
        \\usepackage{import}
        \\usepackage[hidelinks]{hyperref}
        \\usepackage{url}
        \\hypersetup{colorlinks=true, allcolors=blue}
        % Geometry
        \\usepackage[a4paper, width=150mm, top=25mm, bottom=25mm]{geometry}
        \\usepackage{parskip}
        \\setlength{\\parindent}{0pt}
        \\setlength{\\parskip}{\\baselineskip}
        % Math
        \\usepackage{amsmath}
        \\usepackage{amssymb}
        % Tables
        \\usepackage{array}
        \\usepackage{multirow}
        \\usepackage{longtable}
        % Color
        \\usepackage{xcolor}
        % Figures
        \\usepackage{graphicx} % To show figures
        \\usepackage{wrapfig}  % Wrap text around figures
        \\usepackage{subcaption}
        \\usepackage{rotating}
        % Others
        \\usepackage{float}
        \\usepackage{lastpage}
        \\usepackage[normalem]{ulem}
        \\usepackage{capt-of}
        \\usepackage{csquotes}
        \\usepackage{enumitem}
        \\usepackage{ragged2e}
        \\setlist{nosep} % or \setlist{noitemsep} to leave space around whole list
        % TOC and Appendix
        \\usepackage{appendix}
        \\usepackage[nottoc]{tocbibind}
          \\usepackage[acronyms, section]{glossaries}
          \\makeglossaries
        % Footers and Headers
        \\usepackage{fancyhdr}
        \\pagestyle{fancy}
        \\fancyhf{}
        \\fancyfoot[C]{\\thepage}
        \\renewcommand{\\footrulewidth}{0.1pt}
        % Bibliography
        \\usepackage{natbib}
        \\makeatletter
        \\renewcommand{\\maketitle}{%
        \\begingroup\\parindent0pt
        \\Large{\\bfseries\\@title}\\newline
        \\normalsize{\\bfseries\\@author}\\newline
        \\normalsize{\\@date}\\vspace{-0.2cm}\\newline
        \\noindent\\makebox[\\textwidth]{\\rule{\\textwidth}{0.4pt}}
        \\endgroup\\@afterindentfalse\\@afterheading}
        \\makeatother
        [NO-DEFAULT-PACKAGES]
        "
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

          ("report"
           "
        \\documentclass[10pt]{report}
        % Setup
        \\usepackage[english]{babel}
        \\usepackage[utf8]{inputenc}
        \\usepackage{import}
        \\usepackage[hidelinks]{hyperref}
        \\usepackage{url}
        \\hypersetup{colorlinks=false}
        % \\usepackage[none]{hyphenat}
        % Geometry
        \\usepackage[a4paper, width=150mm, top=25mm, bottom=25mm]{geometry}
        \\usepackage{parskip}
        \\setlength{\\parindent}{0pt}
        \\setlength{\\parskip}{\\baselineskip}
        % Math
        \\usepackage{amsmath}
        \\usepackage{amssymb}
        \\usepackage[ruled, vlined]{algorithm2e}
        \\usepackage{mathrsfs}
        % Tables
        \\usepackage{array}
        \\usepackage{multirow}
        \\usepackage{longtable}
        \\usepackage{lscape}
        % Color
        \\usepackage{xcolor}
        % Figures
        \\usepackage{graphicx} % To show figures
        \\usepackage{wrapfig}  % Wrap text around figures
        \\usepackage{caption}
        \\usepackage{subcaption}
        \\usepackage{rotating}
        % others
        \\usepackage{fixltx2e} % Required for \textsubscript
        \\usepackage{float}
        \\usepackage{lastpage}
        \\usepackage[normalem]{ulem}
        \\usepackage{capt-of}
        \\usepackage{csquotes}
        \\usepackage{enumitem}
        \\usepackage{ragged2e}
        \\usepackage{comment}
        \\setlist{nosep} % or \setlist{noitemsep} to leave space around whole list
        % TOC and Appendix
        \\usepackage{appendix}
        \\usepackage[nottoc]{tocbibind}
          \\usepackage[acronyms, section]{glossaries}
          \\makeglossaries
        % Footers and Headers
        \\usepackage{fancyhdr}
        \\pagestyle{fancy}
        \\fancyhf{}
        \\fancyfoot[C]{\\thepage}
        \\renewcommand{\\footrulewidth}{0.1pt}
        % Bibliography
        \\usepackage{natbib}

        [NO-DEFAULT-PACKAGES]
        "

           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))


          ;; Book
          ("book" "\\documentclass[10pt]{book}"
           ("\\part{%s}" . "\\part*{%s}")
           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
          ;; Beamer
          ("beamer"
           "
    \\documentclass[presentation]{beamer}
\\usepackage{xcolor}
    % Bibliography
    \\usepackage{natbib}
        % Math
        \\usepackage{amsmath}
        \\usepackage{amssymb}
        \\usepackage[ruled, vlined]{algorithm2e}
        \\usepackage{mathrsfs}
  \\usepackage{listings}
  \\lstset{frame=single,aboveskip=1em,
          framesep=.5em,backgroundcolor=\\color{blue},
          rulecolor=\\color{blue},framerule=1pt}

  \\newcommand\\basicdefault[1]{\\scriptsize\\color{black}\\ttfamily#1}
  \\lstset{basicstyle=\\basicdefault{\\spaceskip1em}}
  \\lstset{literate=
              {§}{{\\S}}1
              {©}{{\\raisebox{.125ex}{\\copyright}\\enspace}}1
              {«}{{\\guillemotleft}}1
              {»}{{\\guillemotright}}1
              {Á}{{\\'A}}1
              {Ä}{{\\\"A}}1
              {É}{{\\'E}}1
              {Í}{{\\'I}}1
              {Ó}{{\\'O}}1
              {Ö}{{\\\"O}}1
              {Ú}{{\\'U}}1
              {Ü}{{\\\"U}}1
              {ß}{{\\ss}}2
              {à}{{\\`a}}1
              {á}{{\\'a}}1
              {ä}{{\\\"a}}1
              {é}{{\\'e}}1
              {í}{{\\'i}}1
              {ó}{{\\'o}}1
              {ö}{{\\\"o}}1
              {ú}{{\\'u}}1
              {ü}{{\\\"u}}1
              {¹}{{\\textsuperscript1}}1
              {²}{{\\textsuperscript2}}1
              {³}{{\\textsuperscript3}}1
              {ı}{{\\i}}1
              {—}{{---}}1
              {’}{{'}}1
              {…}{{\\dots}}1
              {⮠}{{$\\hookleftarrow$}}1
              {␣}{{\\textvisiblespace}}1,
              keywordstyle=\\color{green}\\bfseries,
              identifierstyle=\\color{red},
              commentstyle=\\color{gary}\\upshape,
              stringstyle=\\color{blue}\\upshape,
              emphstyle=\\color{brown}\\upshape,
              showstringspaces=false,
              columns=fullflexible,
              keepspaces=true}
  [DEFAULT-PACKAGES]
  \\hypersetup{linkcolor=blue,urlcolor=blue,
    citecolor=red,colorlinks=true}
  \\AtBeginDocument{\\renewcommand{\\UrlFont}{\\ttfamily}}
  [PACKAGES]
  [EXTRA]
    "
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

          ))
  ;; -------------------- Bibliography --------------------
  (setq org-latex-prefer-user-labels t)
  (use-package bibtex
    :straight (:type built-in)
    :init
    (use-package ivy-bibtex)
    (setq bibtex-completion-bibliography '("~/Documents/Org/Bibliography/Master.bib"))
    (setq bibtex-completion-library-path nil)
    (setq bibtex-completion-notes-path nil)
    (setq bibtex-completion-pdf-field "file")
    (setq bibtex-completion-pdf-open-function
          (lambda (fpath)
            (call-process "open" nil 0 nil fpath))))

  (use-package org-ref
    :bind
    (:map bibtex-mode-map
          ("H-]" . org-ref-bibtex-hydra/body)
          :map org-mode-map
          ("C-c ]" . org-ref-insert-link)
          ("s-]" . org-ref-insert-link-hydra/body))
    :init
    (use-package org-ref-ivy :straight (:type built-in))
    (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
          org-ref-insert-cite-function 'org-ref-cite-insert-ivy
          org-ref-insert-label-function 'org-ref-insert-label-link
          org-ref-insert-ref-function 'org-ref-insert-ref-link))

  ;; -------------------- PDF --------------------
  (use-package pdf-tools
    :init
    (use-package tablist)

    :mode ("\\.pdf\\'" . pdf-view-mode)
    :bind
    (:map pdf-view-mode-map
          ("C-s" . isearch-forward))
    :config
    (pdf-loader-install)
    (setq pdf-view-display-size 'fit-page)
    )
  (use-package pdf-view-restore
    :after pdf-tools
    :hook
    (pdf-view-mode . pdf-view-restore-mode)
    :config
    (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore")
    )
  ;; -------------------- Org Download --------------------
  ;; https://github.com/abo-abo/org-download
  (use-package org-download
    :config
    (setq org-download-display-inline-images t))
  ;; -------------------- Beautifying Org Mode --------------------
  ;; Emphasis - disable strikethrough
  (setq org-emphasis-alist '(("*" bold)
                             ("/" italic)
                             ("_" underline)
                             ("=" org-verbatim verbatim)
                             ("~" org-code verbatim)
                             ("+" (:strike-through nil))))
  ;; Emphasis - hide markers
  (setq org-hide-emphasis-markers t)
  ;; Org-Superstar - https://github.com/integral-dw/org-superstar-mode
  (use-package org-superstar
    :hook
    (org-mode . (lambda () (org-superstar-mode 1)))
    :config
    (setq org-superstar-headline-bullets-list '("◉" "◈" "○" "▷"))
    ;; Do not cycle after bottom level
    (setq org-superstar-cycle-headline-bullets nil)
    )
  ;; Fonts and Section Title color
  (let* ((variable-tuple
          (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :forground "RoyalBlue1"   :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :foreground "firebrick3" :height 1.25))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :foreground "green3" :height 1.5))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :foreground "DarkOrange2" :height 1.75))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))
  ;; Indentation
  (setq org-startup-indented nil)
  ;; prettify symbols
  (setq org-pretty-entities nil)
  ;; images - set width
  (setq org-startup-with-inline-images t
        org-image-actual-width '(300))
  )

(use-package all-the-icons)

(use-package mode-icons
  :config
  (mode-icons-mode))

(use-package doom-themes
  :config
  (set-face-attribute 'cursor nil :background "DarkRed")

  ;; (load-theme 'doom-gruvbox t)
  ;; (load-theme 'spacemacs-light t)
  ;; (load-theme 'doom-opera-light t)

  (doom-themes-visual-bell-config)  ;; flashing mode-line on errors

  ;; (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  ;; (doom-themes-treemacs-config)

  (doom-themes-org-config)          ;; Corrects (and improves) org-mode's native fontification.
  )

;; (load-theme 'doom-city-lights t))
;; (load-theme 'doom-molokai t)
;; (load-theme 'doom-sourcerer t)
;; (load-theme 'doom-tomorrow-night t)
;; (load-theme 'doom-gruvbox t)

(use-package modus-themes)
(use-package tango-plus-theme)
(use-package base16-theme)
(use-package spacemacs-theme
  :config
  (load-theme 'spacemacs-light t))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  ;; (setq inhibit-compacting-font-caches t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-height 15)
  (setq doom-modeline-vcs-max-length 80))

(defun my/edit-config ()
  "Opens the my-literate-emacs-configuration.org file."
  (interactive)
  (find-file "~/.emacs.d/my-literate-emacs-configuration.org"))

(defun my/open-mo-notes ()
  "Opens Maritime Optima Notes folder"
  (interactive)
  (dired "~/Documents/Work/Maritime-Optima/Notes/"))

(defun window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(bind-key "C-x C-t" 'window-split-toggle)

(use-package tramp
  :straight (:type built-in)
  :config
  (setq tramp-default-method "ssh")
  )
