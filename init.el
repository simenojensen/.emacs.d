;;; init.el --- -*- lexical-binding: t -*-

(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(defvar better-gc-cons-threshold 100000000 ; 100mb
  "The default value to use for `gc-cons-threshold'.
   If you experience freezing, decrease this.  If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
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

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(setq-default package-enable-at-startup nil)

(setq-default load-prefer-newer t)

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package bind-key)

(use-package diminish)

(use-package try)

(setq user-full-name "Simen Omholt-Jensen")
(setq user-mail-address "simen@omholt-jensen.com")

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))                       ;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-appearance . dark))                              ;; Fancy titlebar for MacOS
(setq ns-use-proxy-icon  nil)                                                           ;; Fancy titlebar for MacOS
(setq frame-title-format '(:eval (if (buffer-file-name)                                 ;; Set frame title to *Buffer/File Name*
                                     (abbreviate-file-name (buffer-file-name)) "%b")))
(set-language-environment "UTF-8")                                                      ;; Set enconding language
(set-default-coding-systems 'utf-8)                                                     ;; Set enconding language
(prefer-coding-system 'utf-8)                                                           ;; Set enconding language
(set-terminal-coding-system 'utf-8)                                                     ;; Set enconding language
(set-keyboard-coding-system 'utf-8)                                                     ;; Set enconding language
(global-display-line-numbers-mode)                                                      ;; Display line numbers
(setq-default read-process-output-max (* 1024 1024))                                    ;; Increase the amount of data which Emacs reads from the process
(dolist (mode '(vterm-mode-hook
                jupyter-repl-mode-hook))                                                       ;; disable line number for some modes
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
(setq-default fill-column 80)                                                           ;; Set fill column to 80 chars by default
(setq-default column-number-mode t)                                                     ;; Display column numbers
(dolist (mode '(org-mode-hook                                                           ;; Disable line numbers for some modes
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
(setq-default inhibit-startup-screen t)                                                 ;; Don't show the startup message
(setq inhibit-startup-echo-area-message t)                                              ;; Don't show the startup echo message
(setq-default initial-scratch-message nil)                                              ;; Set initial scratch message to nil
(set-fringe-mode 10)                                                                    ;; Give some breathing room
(set-default 'truncate-lines t)                                                         ;; default truncate lines
(setq debug-on-error nil)                                                               ;; Receive more information errors
(setq custom-file "~/.emacs.d/custom.el")
(ignore-errors (load custom-file))                                                      ;; Load custom.el if it exists
(setq-default create-lockfiles nil)                                                     ;; Disable lock files
(setq-default backup-directory-alist '(("." . "/Users/simenojensen/.emacs.d/backups"))) ;; Save backup files
(setq-default indent-tabs-mode nil)                                                     ;; Don't use hard tabs
(setq echo-keystrokes 0.1)                                                              ;; Echo keystrokes fast
(fset 'yes-or-no-p 'y-or-n-p)                                                           ;; y-or-n instead of yes-or-no
(add-hook 'before-save-hook 'delete-trailing-whitespace)                                ;; Delete trailing whitespace on save
(setq require-final-newline t)                                                          ;; Add a newline at end of file on save
(global-auto-revert-mode t)                                                             ;; Automatically update buffers if a file content has changed on disk

(save-place-mode t)                                                                     ;; Save position of the point in file
(global-hl-line-mode t)                                                                 ;; Highlight the line with the point
(add-hook 'before-save-hook 'time-stamp)                                                ;; Update timestamp of 8 first lines on save
(setq large-file-warning-threshold 100000000)                                           ;; Warn when opening file larger than 100 MB
(desktop-save-mode 1)                                                                   ;; save desktop
(setq history-delete-duplicates t)                                                      ;; delete duplicate history
(setq revert-without-query '(".*"))                                                     ;; do not ask when reverting buffer
(setq-default cursor-type '(bar . 4))                                                   ;; use bar for cursort
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)                                 ;; Cancel on escape

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
       (customize-set-variable 'mac-command-modifier 'meta)
       (customize-set-variable 'mac-right-command-modifier 'super)
       (customize-set-variable 'mac-option-modifier 'alt)
       (customize-set-variable 'mac-right-option-modifier 'hyper)
       (bind-key "M-=" 'text-scale-increase)
       (bind-key "M--" 'text-scale-decrease)
       (bind-key "M-`" 'other-frame)
       (use-package exec-path-from-shell
         :config
         (setq shell-file-name "/opt/homebrew/bin/zsh") ;; Let emacs know which shell to use.
         (setq exec-path-from-shell-variables  '("PATH" "MANPATH" "VIRTUAL_ENV" "PKG_CONFIG_PATH" "GOPATH"))
         (if (string-equal system-type "darwin")
             (exec-path-from-shell-initialize)))
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
(unbind-key "C-v" global-map) ;; disable annoying scroll window

(bind-key "C-x C-l" 'toggle-truncate-lines)

(bind-key "M-p" 'backward-paragraph)
(bind-key "M-n" 'forward-paragraph)
(bind-key "M-g" 'goto-line)

(bind-key "C-x b" 'ibuffer-other-window)
(bind-key "C-x C-b" 'switch-to-buffer)

(unbind-key "C-x f" global-map)

(use-package crux
  :bind
  ("C-a" . crux-move-beginning-of-line)
  :config
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

(use-package ivy
  :diminish
  :init
  (use-package amx)
  (use-package counsel :diminish :config (counsel-mode 1))
  (use-package swiper)
  (ivy-mode 1)
  :bind
  (("C-x C-f" . counsel-find-file)
   ("C-x f". counsel-fzf)
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
  ;; (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t))

(use-package color-rg
  :straight
  (color-rg :type git :host github :repo "manateelazycat/color-rg")
  :bind
  ("C-M-s" . color-rg-search-input))

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
  ;; Enable global auto-revert
  (global-auto-revert-mode t)
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

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)))

(use-package dired-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package disk-usage)

(use-package restart-emacs)

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
(use-package multi-vterm)

(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package projectile
  :diminish
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-expand-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-litter-directories            '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package format-all
  :hook
  (prog-mode . format-all-mode)
  (format-all-mode . format-all-ensure-formatter)
  )

(use-package evil-nerd-commenter
  :bind
  ("C-;" . evilnc-comment-or-uncomment-lines))

(use-package yasnippet
  :diminish yas-minor-mode
  :init (use-package yasnippet-snippets :after yasnippet)
  :config
  (yas-global-mode 1))

(use-package flycheck
  :diminish
  :init
  (global-flycheck-mode)
  :hook
  (prog-mode . flycheck-mode)
  :config
  (setq flycheck-checker-error-threshold 1000)
  ;; (setq-default flycheck-c/c++-clang-executable "/usr/bin/clangd")
  ;; (setq-default flycheck-clang-standard-library "libc++")
  (setq-default flycheck-clang-language-standard "c++20")
  (setq-default flycheck-cppcheck-standards '("c++20"))
  (setq-default flycheck-clang-args "-std=c++20")
  )

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (html-mode . lsp-deferred)
         (json-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . (lambda ()
                       (bind-key "M-;" 'lsp-rename lsp-mode-map))))
  :commands lsp
  :config
  (setq lsp-idle-delay 0.5)
  (setq lsp-log-io nil) ; if set to true can cause a performance hit
  ;; symbol highlighting
  (setq lsp-enable-symbol-highlighting t)
  ;; lenses
  (setq lsp-lens-enable nil)
  ;; headerline
  (setq lsp-headerline-breadcrumb-enable t)
  ;; modeline
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable t)
  ;; linter
  (setq lsp-diagnostics-provider :auto) ;; prefer flycheck, fallback to flymake
  ;; eldoc
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-eldoc-render-all t)
  ;; signatures
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  ;; completion
  (setq lsp-completion-provider :capf)
  (setq lsp-completion-show-detail t)
  (setq lsp-completion-show-kind t)
  )

(use-package lsp-ui
  :commands lsp-ui-mode
  :bind
  ;; lsp-ui-peek
  ((:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references)
         ("C-c d" . lsp-ui-doc-show)
         ))
  :config
  ;; show docs
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse t)
  ;; sideline
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-diagnostics t)
  )
         ;; lsp-ui-doc
  ;;        ("M-i" . lsp-ui-doc-focus-frame))
  ;;  ("s-i" . my/toggle-lsp-ui-doc))
  ;; :preface
  ;; (defun my/toggle-lsp-ui-doc ()
  ;;   (interactive)
  ;;   (if lsp-ui-doc-mode
  ;;       (lsp
  ;;         (progn-ui-doc-mode -1)
  ;;         (lsp-ui-doc--hide-frame))
  ;;     (lsp-ui-doc-mode 1))))

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package company
  :diminish company-mode
  :hook
  (after-init . global-company-mode)
  :bind
  ((:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
   (:map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)))
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0)
  (setq company-echo-delay 5)
  ;; (setq company-tooltip-idle-delay 0.0)
  ;; (setq company-tooltip-align-annotations t)
  (setq company-require-match nil)
  (setq company-show-numbers t)
  (setq company-dabbrev-downcase nil) ;; case insensitive for dabbrev backend
  (global-company-mode 1)
  ;; Don't use company in debugger mode
  (setq company-global-modes '(not gud-mode)))

(use-package company-box
  :diminish
  :hook
  (company-mode . company-box-mode)
  :config
  (setq company-box-doc-enable t)
  (setq company-box-doc-delay 0.2)
  )

(use-package smartparens
  :init
  (progn
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1))
  :config
  (progn
    (setq smartparens-strict-mode t)
    (setq sp-show-pair-from-inside nil)
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))))

(defun my/jupyter-load-file ()
  "Send current buffer to jupyter kernel by default"
  (interactive)
  (jupyter-load-file (buffer-file-name)))

(use-package python
  :hook
  (python-mode . (lambda () ;; emulate python-shell-send-buffer
                   (setq indent-tabs-mode nil)
                   (display-fill-column-indicator-mode) ;; display column
                   (unbind-key "C-c C-l" jupyter-repl-interaction-mode-map)
                   (bind-key "C-c C-c" 'my/jupyter-load-file jupyter-repl-interaction-mode-map)
                   (add-hook 'before-save-hook #'lsp-format-buffer t t)))
  :config
  ;; silence indentation guesses
  (setq python-indent-guess-indent-offset-verbose nil))

(use-package lsp-pyright)

(use-package conda
  :hook
  (python-mode . (lambda () (conda-env-activate "base")))
  :config
  (conda-env-initialize-interactive-shells) ;; interactive shell support
  ;; (conda-env-initialize-eshell)             ;; eshell support
  ;; (conda-env-autoactivate-mode t)           ;; autoactivate
  (setq conda-env-home-directory "/opt/homebrew/Caskroom/miniconda/base/")
  (setq conda-anaconda-home "/opt/homebrew/Caskroom/miniconda/base/"))

(use-package numpydoc
  :after python
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate))
  :config
  (setq numpydoc-insert-examples-block nil))

(use-package jupyter
  :bind
  (:map python-mode-map
        ("C-c C-p" . jupyter-run-repl))
  :init
  (setq jupyter-repl-allow-RET-when-busy t)
  (setq jupyter-repl-echo-eval-p t)) ;; show plots

(use-package cython-mode)

(use-package lsp-java)

(use-package go-mode
  :hook
  (go-mode . (lambda()
               (add-hook 'before-save-hook #'lsp-format-buffer t t)
               (add-hook 'before-save-hook #'lsp-organize-imports t t))))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package rtf-mode
  :config
  (add-to-list 'auto-mode-alist
               '("\\.rtf$" . rtf-mode))
  )

(use-package rjsx-mode
  :mode
  (("\\.js\\'" . rjsx-mode)
   ("\\.tsx\\'" . rjsx-mode)
   ("\\.ts\\'" . rjsx-mode)))

(defun my/setup-tide-mode()
  "Setup function for tide"
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1))

(use-package tide
  :after
  (rjsx-mode)
  :hook
  (rjsx-mode . my/setup-tide-mode))

(use-package prettier-js
  :after
  (rjsx-mode)
  :hook
  (rjsx-mode . prettier-js-mode))

(use-package css-mode
  :hook
  (css-mode . lsp))

(use-package json-mode
  :mode "\\.json\\'"
  :hook
  (json-mode . lsp))

(use-package yaml-mode
  :mode
  ("\\.ya?ml\\'"))

(use-package csv-mode
  :mode "\\.[Cc][Ss][Vv]\\'"
  ;; :init (setq csv-separators '("," ";" "|" " "))
  :config
  (setq csv-separators '(",")))

(use-package all-the-icons)

(use-package all-the-icons-ivy-rich
  :config
  (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package all-the-icons-dired
  :diminish
  :custom-face
  (all-the-icons-dired-dir-face ((t (:foreground nil))))
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package mode-icons
  :config
  (mode-icons-mode))

(use-package doom-themes
  :custom-face
  (cursor ((t (:background "DarkRed"))))
  :config
  (load-theme 'doom-gruvbox t)
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

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  ;; (setq inhibit-compacting-font-caches t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-height 15))

(use-package beacon
  :config
  (beacon-mode 1)
  (setq beacon-color "#39FF14"))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package dimmer
  :config
  (setq dimmer-fraction 0)
  (dimmer-mode t))

(use-package flyspell
  :config
  (setenv
   "DICPATH"
   (concat (getenv "HOME") "/Library/Spelling"))
  (setenv "DICTIONARY" "en_US")
  ;; Tell ispell-mode to use hunspell.
  (setq ispell-program-name "hunspell")
  (setq-default ispell-hunspell-dict-paths-alist
                '(("en_US" "~/Library/Spelling/en_US.aff")
                  ("nb" "~/Library/Spelling/nb.aff")
                  )))

(defun my/save-word-to-personal-dictionary ()
  "Save word to personal dictionary"
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

;; Remap
(unbind-key "C-c $" flyspell-mode-map)
(bind-key "C-c $" 'my/save-word-to-personal-dictionary flyspell-mode-map)

;; Norsk tastatur
(bind-key "C-ø" 'flyspell-auto-correct-previous-word flyspell-mode-map)

(use-package synosaurus
  :init
  (synosaurus-mode)
  :hook
  (text-mode . synosaurus-mode)
  :config
  (setq synosaurus-choose-method 'ido))

(use-package langtool
  :init
  (setq langtool-default-language "en-US")
  (setq langtool-bin "/opt/homebrew/bin/languagetool")
  )

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

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
               "* TODO %?\n")
              ("E" "Exam" entry
               (file+headline "~/Documents/Org/Academic.org" "Exams")
               "* TODO %?\n")
              ("P" "Project" entry
               (file+headline "~/Documents/Org/Academic.org" "Projects")
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
    \\hypersetup{colorlinks=false}
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
    \\usepackage{subcaption}
    \\usepackage{rotating}
    % others
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
\\usepackage{listings}
\\lstloadlanguages{XML,XSLT}
\\lstset{defaultdialect=XSLT,frame=single,
	framesep=.5em,backgroundcolor=\\color{AliceBlue},
	rulecolor=\\color{LightSteelBlue},framerule=1pt}
\\usepackage{xcolor}
\\newcommand\\basicdefault[1]{\\scriptsize\\color{Black}\\ttfamily#1}
\\lstset{basicstyle=\\basicdefault{\\spaceskip.5em}}
\\lstset{literate=
	    {§}{{\\S}}1
	    {©}{{\\raisebox{.125ex}{\\copyright}\\enspace}}1
	    {«}{{\\guillemotleft}}1
	    {»}{{\\guillemotright}}1
	    {’}{{'}}1
	    {…}{{\dots}}1
	    keywordstyle=\\color{DarkGreen}\bfseries,
	    identifierstyle=\\color{DarkRed},
	    commentstyle=\\color{Gray}\\upshape,
	    stringstyle=\\color{DarkBlue}\\upshape,
	    emphstyle=\\color{Chocolate}\\upshape,
	    showstringspaces=false,
	    columns=fullflexible,
	    keepspaces=true}

  [DEFAULT-PACKAGES]
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

(defun my/get-file-content-as-string (filePath)
  "Return filePath's content as string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun my/edit-config ()
  "Opens the my-literate-emacs-configuration.org file."
  (interactive)
  (find-file "~/.emacs.d/my-literate-emacs-configuration.org"))

(defun my/bib ()
  "Opens the master bibliography file."
  (interactive)
  (find-file "~/Documents/Org/Bibliography/Master.bib"))

(defun my/jupyter-refresh-kernelspecs ()
  "Refresh Jupyter kernelspecs"
  (interactive)
  (jupyter-available-kernelspecs t))

(defun my/org-startup ()
  (interactive)
  (insert "#+TITLE: \n#+AUTHOR: Simen Omholt-Jensen\n#+OPTIONS: toc:nil\n"))

(defun my/org-notebook-startup ()
  (interactive)
  (insert "#+TITLE: \n#+AUTHOR: Simen Omholt-Jensen\n#+OPTIONS: toc:nil\n#+STARTUP: overview\n")
  (insert "#+PROPERTY: header-args:python :session notebook :kernel notebook :async yes :results output :exports both :eval never-export\n\n")
  (insert "** Imports\n #+begin_src python\n")
  (insert "  import numpy as np\n  import pandas as pd\n  import matplotlib.pyplot as plt\n")
  (insert "  import matplotlib as mpl\n  mpl.rcParams['figure.facecolor']='w'\n")
  (insert "  #+end_src\n\n"))

(defun my/browse-url-of-buffer-with-chrome ()
  "Same as `browse-url-of-buffer` but using chrome"
  (interactive)
  ;; (shell-command (concat "open -a 'Google Chrome.app' file://" buffer-file-name)))
 (shell-command (concat "open -a 'Google Chrome.app' " buffer-file-name)))

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

(use-package google-this
  :diminish
  :config
  (google-this-mode t))

(use-package engine-mode
  :config
  (engine-mode t)
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s")
  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :docstring "Mappin' it up.")
  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y"))

(use-package wgrep)

(use-package tramp
  :straight (:type built-in)
  :config
  (setq tramp-default-method "ssh")
  )


