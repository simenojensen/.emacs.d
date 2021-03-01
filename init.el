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

(require 'package)

(setq-default package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                                 ("melpa" . "https://melpa.org/packages/")
                                 ("org" . "https://orgmode.org/elpa/")))

(package-initialize)

(setq-default package-enable-at-startup nil)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :config
  (setq-default auto-package-update-interval 7) ;; in days
  (setq-default auto-package-update-delete-old-versions t)
  (setq-default auto-package-update-hide-results t)
  (auto-package-update-maybe))

(setq-default load-prefer-newer t)

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(require 'bind-key)

(use-package diminish)

(use-package try)

(setq user-full-name "Simen Omholt-Jensen")
(setq user-mail-address "simen@omholt-jensen.com")

(setq frame-title-format '(:eval (if (buffer-file-name)                                 ;; Set frame title to *Buffer/File Name*
                                     (abbreviate-file-name (buffer-file-name)) "%b")))
(set-language-environment "UTF-8")                                                      ;; Set enconding language
(set-default-coding-systems 'utf-8)                                                     ;; Set enconding language
(prefer-coding-system 'utf-8)                                                           ;; Set enconding language
(set-terminal-coding-system 'utf-8)                                                     ;; Set enconding language
(set-keyboard-coding-system 'utf-8)                                                     ;; Set enconding language
(global-display-line-numbers-mode)                                                      ;; Display line numbers
(dolist (mode '(vterm-mode-hook
                jupyter-repl-mode-hook))                                                       ;; disable line number for some modes
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
(setq column-number-mode t)                                                             ;; Display column numbers
(setq-default inhibit-startup-screen t)                                                 ;; Don't show the startup message
(setq-default initial-scratch-message nil)                                              ;; Set initial scratch message to nil
(set-fringe-mode 10)                                                                    ;; Give some breathing room
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
(setq-default cursor-type '(bar . 4))                                                         ;; use bar for cursort

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

(use-package crux
  :bind
  ("C-a" . crux-move-beginning-of-line)
  :config
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

(use-package ivy
  :diminish
  :init
  (use-package amx :defer t)
  (use-package counsel :diminish :config (counsel-mode 1))
  (use-package swiper :defer t)
  (ivy-mode 1)
  :bind
  (("C-x C-f" . counsel-find-file)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-h l" . counsel-find-library)
   ("C-h i" . counsel-info-lookup-symbol)
   ("C-h u" . counsel-unicode-char)
   ("M-x" . counsel-M-x)
   ("M-v" . counsel-yank-pop)
   ("C-x C-b" . counsel-switch-buffer)
   ("C-s" . swiper-isearch))
  :config
  (ivy-mode 1)
  (setq ivy-height 20)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-display-style 'fancy)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t))

(use-package color-rg
  :load-path (lambda () (expand-file-name "site-elisp/color-rg" user-emacs-directory))
  :bind
  ("C-M-s" . color-rg-search-input))

(use-package dired
  :ensure nil
  :bind
  (("C-x C-j" . dired-jump)
   ("C-x j" . dired-jump-other-window))
  :custom
  ;; Always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Auto refresh Dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  ;; Quickly copy/move file in Dired
  (dired-dwim-target t)
  ;; Move files to trash when deleting
  (delete-by-moving-to-trash t)
  ;; Load the newest version of a file
  (load-prefer-newer t)
  ;; Detect external file changes and auto refresh file
  (auto-revert-use-notify nil)
  (auto-revert-interval 3) ; Auto revert every 3 sec
  :config
  ;; Enable global auto-revert
  (global-auto-revert-mode t)
  ;; sort directory first
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq dired-listing-switches "-laXGh --group-directories-first")
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil)
  :hook
  (dired-mode . (lambda ()
                  (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file)
                  (local-set-key (kbd "RET") #'dired-find-alternate-file)
                  (local-set-key (kbd "^")
                                 (lambda () (interactive) (find-alternate-file ".."))))))

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

(cond ((eq system-type 'darwin)
       (customize-set-variable 'mac-command-modifier 'meta)
       (customize-set-variable 'mac-option-modifier 'alt)
       (customize-set-variable 'mac-right-command-modifier 'super)
       (bind-key "M-=" 'text-scale-increase)
       (bind-key "M--" 'text-scale-decrease)
       (use-package exec-path-from-shell
         :defer nil
         :config
         (setq shell-file-name "/usr/local/bin/zsh") ;; Let emacs know which shell to use.
         (setq exec-path-from-shell-variables  '("PATH" "MANPATH" "VIRTUAL_ENV" "PKG_CONFIG_PATH"))
         (if (string-equal system-type "darwin")
             (exec-path-from-shell-initialize)))
       )
      ((eq system-type 'windows-nt)
       
       )
      ((eq system-type 'gnu/linux)
       
       ))

(bind-key "M-`" 'other-frame)

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
  )

(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package projectile
  :diminish
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package dumb-jump
  :bind
  (:map prog-mode-map
        (("C-c C-o" . dumb-jump-go-other-window)
         ("C-c C-j" . dumb-jump-go)
         ("C-c C-i" . dumb-jump-go-prompt)))
  :custom (dumb-jump-selector 'ivy))

(use-package iedit
  :bind
  ("M-;" . iedit-mode))

(use-package format-all)

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
  :hook
  (prog-mode . flycheck-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package lsp-mode
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-sqls-server "~/.go/bin/sqls")
  :config
  ;; debug info
  (setq lsp-print-io t)
  (setq lsp-print-performance t)
  ;; general
  (setq lsp-keymap-prefix "s-l")                         ;; set keymap
  (setq lsp-prefer-capf t)                               ;; use company-capf - recommended over company-lsp
  (setq lsp-keep-workspace-alive nil)                    ;; close workspace when no files
  (setq lsp-enable-snippet t)                            ;; enable snippet completion
  (setq lsp-auto-guess-root nil)                         ;; set project files manually
  (setq lsp-restart 'auto-restart)                       ;; restart if server exits
  (setq lsp-document-sync-method nil)                    ;; use default method recommended by server. 'incremental 'full
  (setq lsp-response-timeout 10)                         ;; default timeout val
  (setq lsp-auto-configure t)                            ;; let lsp-mode autoconfigure company etc
  (setq lsp-enable-completion-at-point t)                ;; enable completion-at-point
  (setq lsp-diagnostic-package :flycheck)                ;; use flycheck for syntax highlighting
  (setq lsp-enable-indentation t)                        ;; indent regions based on lsp
  (setq lsp-signature-auto-activate nil)                 ;; don't display documentation in minibuffer
  (setq read-process-output-max (* 1024 1024))           ;; 1mb
  (setq lsp-idle-delay 0.5))                             ;; lsp refresh rate

(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :bind
  ;; lsp-ui-peek
  ((:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references)
         ;; lsp-ui-doc
         ("M-i" . lsp-ui-doc-focus-frame))
   ("s-i" . my/toggle-lsp-ui-doc))
  :config
  ;; lsp-ui-sideline
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-ignore-duplicate t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-show-symbol nil)
  (setq lsp-ui-sideline-delay 0.5)
  ;; lsp-ui-doc
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-delay 0)
  (setq lsp-ui-doc-max-height 100)
  (setq lsp-ui-doc-max-width 400)
  (setq lsp-ui-doc-use-childframe t)
  (setq lsp-ui-doc-use-webkit nil)
  :preface
  (defun my/toggle-lsp-ui-doc ()
    (interactive)
    (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
      (lsp-ui-doc-mode 1)))
  :hook
  (lsp-mode . lsp-ui-mode))

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
  (setq company-minimum-prefix-length 3)
  (setq company-idle-delay 0.2)
  (setq company-echo-delay 0.2)
  (setq company-tooltip-idle-delay 0.2)
  (setq company-tooltip-align-annotations t)
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
  :defines company-box-icons-all-the-icons
  :init (setq company-box-backends-colors nil
              company-box-highlight-prefix t)
  :config
  (setq company-box-doc-delay 0)
  (setq company-box-enable-icon t)
  (setq company-box-color-icons nil)
  (setq company-box-max-candidates 10)
  (setq company-box-show-single-candidate t)
  ;; all-the-icons-integration
  (with-no-warnings
    ;; Prettify icons
    (defun my-company-box-icons--elisp (candidate)
      (when (derived-mode-p 'emacs-lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp))

  (declare-function all-the-icons-faicon 'all-the-icons)
  (declare-function all-the-icons-material 'all-the-icons)
  (declare-function all-the-icons-octicon 'all-the-icons)
  (setq company-box-icons-all-the-icons
        `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
          (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
          (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
          (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
          (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
          (Field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
          (Variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
          (Class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
          (Interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
          (Module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
          (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
          (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
          (Value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
          (Enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
          (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
          (Snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
          (Color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
          (File . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
          (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
          (Folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
          (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
          (Constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
          (Struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
          (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
          (Operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
          (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
          (Template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15)))
        company-box-icons-alist 'company-box-icons-all-the-icons))

(defun my/jupyter-load-file ()
  "Send current buffer to jupyter kernel by default"
  (interactive)
  (jupyter-load-file (buffer-file-name)))

(use-package python
  :hook
  (python-mode . (lambda () ;; emulate python-shell-send-buffer
                   (unbind-key "C-c C-l" jupyter-repl-interaction-mode-map)
                   (bind-key "C-c C-c" 'my/jupyter-load-file jupyter-repl-interaction-mode-map)))
  :config
  ;; silence indentation guesses
  (setq python-indent-guess-indent-offset-verbose nil))

(use-package lsp-python-ms
  :diminish
  :init
  (setq lsp-python-ms-auto-install-server t)
  :config
  (setq lsp-python-ms-executable
        "~/.emacs.d/site-elisp/python-language-server/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer"))

(use-package jupyter
  :bind
  (:map python-mode-map
        ("C-c C-p" . jupyter-run-repl))
  :init
  (setq jupyter-repl-echo-eval-p t))

(use-package conda
  :hook
  (python-mode . (lambda () (conda-env-activate "py3")))
  :config
  (conda-env-initialize-interactive-shells) ;; interactive shell support
  ;; (conda-env-initialize-eshell)             ;; eshell support
  ;; (conda-env-autoactivate-mode t)           ;; autoactivate
  (setq conda-env-home-directory "/usr/local/Caskroom/miniconda/base/")
  (setq conda-anaconda-home "/usr/local/Caskroom/miniconda/base/"))

(use-package py-autopep8
  :config
  (setq py-autopep8-options '("--max-line-length=80")))

(use-package lsp-java
  :hook
  (java-mode . lsp))

(defun my/java-one-click-run ()
  "Go to previous window after running"
  (interactive)
  (java-one-click-run)
  (select-window (previous-window)))

(use-package java-one-click-run
  :load-path "~/.emacs.d/site-elisp/java-one-click-run/"
  :init (use-package shell-here)
  :hook
  (java-mode . (lambda () ;; compile and run java program with C-c C-c
                 (unbind-key "C-c C-C" lsp-mode-map)
                 (bind-key "C-c C-c" 'my/java-one-click-run java-mode-map))))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :hook
  (markdown-mode . (lambda ()
                     (remove-hook 'before-save-hook 'delete-trailing-whitespace t)))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package grip-mode
  ;; :init
  ;; (progn
  ;;   (require 'auth-source)
  ;;   (let ((credential (auth-source-user-and-password "api.github.com")))
  ;;     (setq grip-github-user (car credential)
  ;;           grip-github-password (cadr credential))))
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)))



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

(use-package mhtml-mode
  :hook
  (mhtml-mode . (lambda()
                  (lsp)
                  (unbind-key "C-c C-v" mhtml-mode-map)
                  (bind-key "C-c C-v" 'my/browse-url-of-buffer-with-chrome))))

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

;; (use-package spacemacs-theme
;;   :config
;;   (load-theme spacemacs-theme-light))
;; ;; (use-package solarized-theme)
;; (use-package darktooth-theme)
;; (use-package kaolin-themes)
;; (use-package gruvbox-theme
;;   :config
;;   (load-theme 'gruvbox))

(use-package beacon
  :config
  (beacon-mode 1)
  (setq beacon-color "#39FF14"))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package dimmer
  :config
  (setq dimmer-fraction 0.5)
  (dimmer-mode t))

;; Dependency
(use-package page-break-lines)

(use-package dashboard
  :disabled
  :config
  (setq show-week-agenda-p t)
  (dashboard-setup-startup-hook))

(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  ;; Enable document parsing to get support for Latex packages
  (setq TeX-auto-save t)  ;; enable parsing on load
  (setq TeX-parse-self t) ;; enable parsing on save
  (setq-default TeX-master nil) ;; make AUCTeX aware of multi-file document structure
  (setq TeX-view-program-selection '((output-pdf "pdf-tools")))
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  (setq LaTeX-electric-left-right-brace t)
  (setq LaTeX-math-menu-unicode t)
  :hook
  (LaTeX-mode . (lambda ()
                  (rainbow-delimiters-mode)
                  (company-mode)
                  (turn-on-reftex)
                  (setq reftex-plug-into-AUCTeX t)
                  (reftex-isearch-minor-mode)
                  (turn-on-auto-fill) ;; insert automatically fill and indent linebreaks
                  (setq TeX-PDF-mode t)
                  (setq TeX-source-correlate-mode t)
                  (setq TeX-source-correlate-method 'synctex)
                  (setq TeX-source-correlate-start-server t)
                  (pdf-tools-install))) ;; use PDF-tools
  (LaTeX-mode . LaTeX-math-mode) ;; use latex math mode by default
  ;; automatically insert '\(...\)' in Latex files by pressing $
  (LaTeX-mode . (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
                                (cons "\\(" "\\)"))))
  (TeX-after-TeX-LaTeX-command-finished . TeX-revert-document-buffer))

(use-package auctex-latexmk
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package company-auctex
  :init
  (company-auctex-init))

(use-package cdlatex
  :diminish org-cdlatex-mode)

(use-package pdf-tools
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (pdf-tools-install :no-query))

(use-package pdf-view-restore
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))

(use-package org
  :ensure org-plus-contrib
  :pin org
  :hook
  (after-save . my/tangle-emacs-config)
  (org-mode . (lambda ()
                (setq-local company-idle-delay 0.4) ;; company idle delay
                ;; (visual-line-mode) ;; visual line
                (flyspell-mode) ;; turn on flyspell
                (turn-on-org-cdlatex) ;; turn on cdlatex
                (diminish 'org-cdlatex-mode) ;; remove from modeline
                ;; (variable-pitch-mode -1)
                (bind-key "<s-return>" 'org-table-insert-row orgtbl-mode-map)
                ))
  :config
  ;; Org settings
  (setq org-directory "~/Documents/Org") ;; Set default org directory
  (setq org-default-notes-file (concat org-directory "/tasks.org")) ;; Set default org capture file
  ;; Org agenda
  (setq org-todo-keywords
        '((sequence "TODO"  "|" "DONE" "CANCELED")))
  (setq org-agenda-files '("~/Documents/Org/"))
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
  ;; Tangle on saving this file
  (defun my/tangle-emacs-config ()
    "If the current file is this file, the code blocks are tangled"
    (when (equal (buffer-file-name) (expand-file-name "~/.emacs.d/my-literate-emacs-configuration.org"))
      (org-babel-tangle nil "~/.emacs.d/init.el")))
  ;; Do not confirm when evaluating code blocks
  (setq org-confirm-babel-evaluate nil)
  ;; Run/highlight code using babel in org-mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (latex . t)
     (sql . t)
     (shell . t)
     (emacs-lisp . t)))
  ;; Set org emphasis alist - remove strikethroug
  (setq org-emphasis-alist '(("*" bold)
                             ("/" italic)
                             ("_" underline)
                             ("=" org-verbatim verbatim)
                             ("~" org-code verbatim)
                             ("+" (:strike-through nil))))

  (setq org-src-fontify-natively t) ;; Syntax highlight in #+BEGIN_SRC blocks
  (setq org-special-ctrl-a/e t) ;; cycle C-e and C-a
  (setq org-src-window-setup 'current-window) ;; use same window
  (setq org-adapt-indentation nil) ;; do not indent after sections
  ;; ;; edit block inserts
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
  ;; Configure latex exports
  (setq org-latex-logfiles-extensions (quote ("lof" "lot" "xdv" "synctex.gz" "tex" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "pygtex" "pygstyle")))
  (setq org-latex-remove-logfiles t)
  ;; https://so.nwalsh.com/2020/01/05-latex
  (setq org-latex-compiler "xelatex")
  (setq org-latex-pdf-process
        (list (concat "latexmk -"
                      org-latex-compiler
                      " -recorder -synctex=1 -bibtex-cond %b")))
  (setq org-export-in-background t) ;; export async
  ;; Configure Org to use lstlisting for source environments.
  (setq org-latex-listings t)
  ;; Captions below
  (setq org-latex-caption-above nil)
  ;; org ref labels
  (setq org-latex-prefer-user-labels t)
  ;; Use predefine latex template for orgmode export to latex
  ;; https://so.nwalsh.com/2020/01/05-latex
  (setq org-latex-default-packages-alist
        '(
          ;; packages from template creator
          ("" "longtable" nil)
          ("normalem" "ulem" t)
          ("" "textcomp" t)
          ("" "capt-of" nil)
          ("" "hyperref" nil)
          ;; images/figures
          ("" "graphicx" t)
          ("" "grffile" t)
          ("" "wrapfig" t)
          ("" "float" t)
          ("" "rotating" nil)
          ;; tables
          ("" "array" t)
          ("" "tabu" t)
          ("" "multirow" t)
          ("" "tabularx" t)
          ;; math
          ("" "amsmath" t)
          ("" "amssymb" t)
          ("" "amsfonts" t)
          ("" "amsthm" t)
          ("" "relsize" t)
          ("" "mathtools" t)
          ))
  (setq org-latex-classes
        '(("article"
           " \\RequirePackage{fix-cm}
  \\PassOptionsToPackage{svgnames}{xcolor}
  \\documentclass[11pt]{article}
  \\usepackage{fontspec}
  \\usepackage{enumitem}
  \\usepackage[nottoc]{tocbibind}
  \\setlist{nosep,after=\\vspace{4pt}}
  \\usepackage{listings}
  \\lstset{frame=single,aboveskip=1em,
          framesep=.5em,backgroundcolor=\\color{AliceBlue},
          rulecolor=\\color{LightSteelBlue},framerule=1pt}
  \\usepackage{xcolor}
  \\newcommand\\basicdefault[1]{\\scriptsize\\color{Black}\\ttfamily#1}
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
              keywordstyle=\\color{DarkGreen}\\bfseries,
              identifierstyle=\\color{DarkRed},
              commentstyle=\\color{Gray}\\upshape,
              stringstyle=\\color{DarkBlue}\\upshape,
              emphstyle=\\color{Chocolate}\\upshape,
              showstringspaces=false,
              columns=fullflexible,
              keepspaces=true}
  \\usepackage[a4paper,top=2.5cm, bottom=2.5cm, left=2.5cm, right=2.5cm]{geometry}
  \\usepackage{parskip}
  \\setlength\\parindent{0pt}
  \\setlength\\parskip{0pt}
  \\makeatletter
  \\renewcommand{\\maketitle}{%
  \\begingroup\\parindent0pt
  \\Large{\\bfseries\\@title}\\newline
  \\normalsize{\\bfseries\\@author}\\newline
  \\normalsize{\\@date}\\vspace{-0.2cm}\\newline
  \\noindent\\makebox[\\textwidth]{\\rule{\\textwidth}{0.4pt}}
  \\endgroup\\@afterindentfalse\\@afterheading}
  \\makeatother
  [DEFAULT-PACKAGES]
  \\let\\oldtextbf\\textbf
  \\renewcommand{\\textbf}[1]{\\textcolor{red}{\\oldtextbf{#1}}}
  \\renewcommand{\\baselinestretch}{1.0}
  \\hypersetup{linkcolor=Blue,urlcolor=DarkBlue,
    citecolor=DarkRed,colorlinks=true}
  [PACKAGES]
  [EXTRA]"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
  
          ("report" "\\documentclass[11pt]{report}"
           ("\\part{%s}" . "\\part*{%s}")
           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
  
          ("book" "\\documentclass[11pt]{book}"
           ("\\part{%s}" . "\\part*{%s}")
           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t) ;; hide emphasis markers *...*, /.../, etc
  ;; proportional fonts, in different sizes, for the headlines.
  ;; https://edwardtufte.github.io/et-book/
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
  
  ;; (custom-theme-set-faces
  ;;  'user
  ;;  '(variable-pitch ((t (:family "ETBembo" :height 180))))
  ;;  '(fixed-pitch ((t ( :family "Fira Code Retina" :height 160)))))
  
  ;; (custom-theme-set-faces
  ;;  'user
  ;;  '(org-block ((t (:inherit fixed-pitch))))
  ;;  '(org-code ((t (:inherit (shadow fixed-pitch)))))
  ;;  '(org-document-info ((t (:foreground "dark orange"))))
  ;;  '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  ;;  '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
  ;;  '(org-link ((t (:foreground "royal blue" :underline t))))
  ;;  '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;;  '(org-property-value ((t (:inherit fixed-pitch))) t)
  ;;  '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;;  '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
  ;;  '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
  ;;  '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
  )

(use-package org-download
  :custom
  (org-download-display-inline-images nil))

(use-package toc-org
  :after org
  :hook
  (org-mode . toc-org-enable))

(use-package org-bullets
  :after org
  :hook
  (org-mode . org-bullets-mode))

(defun my/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :disabled
  :hook (org-mode . my/org-mode-visual-fill))

(use-package ox-twbs)

(use-package ox-reveal
  :ensure ox-reveal
  :config
  (setq org-reveal-root "/Users/simenojensen/.emacs.d/site-elisp/reveal.js/")
  (setq org-reveal-mathjax t))

(use-package htmlize)

(use-package reftex
  :diminish
  :commands turn-on-reftex
  :config
  (setq reftex-cite-prompt-optional-args t) ;; Prompt for empty optional arguments in cite
  (setq reftex-default-bibliography '("/Users/simenojensen/Documents/Org/Bibliography/library.bib"))
  (setq reftex-plug-into-AUCTeX t))

(use-package ivy-bibtex
  :config
  (setq bibtex-completion-bibliography "/Users/simenojensen/Documents/Org/Bibliography/library.bib") ;; location of bibtex file
  (setq bibtex-completion-library-path "/Users/simenojensen/Documents/Org/Bibliography") ;; directory of bibtex pdf files
  (setq bibtex-completion-notes-path "/Users/simenojensen/Documents/Org/Bibliography/notes.org") ;; location of bibliography notes file
  (setq bibtex-completion-pdf-field "File") ;; using bibtex path reference to pdf file
  ;; open pdf with system pdf viewer (works on mac)
  (setq bibtex-completion-pdf-open-function (lambda (fpath)
                                              (start-process "open" "*open" "open" fpath)))
  (setq ivy-bibtex-default-action 'bibtex-completion-insert-citation))

(use-package org-ref
  :after org
  :config
  (setq org-ref-bibliography-notes "/Users/simenojensen/Documents/Org/Bibliography/notes.org") ;; bibtex notes file
  (setq org-ref-default-bibliography '("/Users/simenojensen/Documents/Org/Bibliography/library.bib")) ;; bibtex file
  (setq org-ref-pdf-directory "/Users/simenojensen/Documents/Org/Bibliography")) ;; bibliography pdf folder

(use-package flyspell
  :config
  (setenv
   "DICPATH"
   (concat (getenv "HOME") "/Library/Spelling"))
  (setenv "DICTIONARY" "en_US")
  ;; Tell ispell-mode to use hunspell.
  (setq ispell-program-name "hunspell")
  (setq-default ispell-hunspell-dict-paths-alist
                '(("en_US" "~/Library/Spelling/en_US.aff"))))

(defun my/get-file-content-as-string (filePath)
  "Return filePath's content as string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun my/edit-config ()
  "Opens the my-literate-emacs-configuration.org file."
  (interactive)
  (find-file "~/.emacs.d/my-literate-emacs-configuration.org"))

(defun my/org-startup ()
  (interactive)
  (insert "#+TITLE: \n#+AUTHOR: Simen Omholt-Jensen\n#+OPTIONS: toc:nil\n"))

(defun my/browse-url-of-buffer-with-chrome ()
  "Same as `browse-url-of-buffer` but using chrome"
  (interactive)
  ;; (shell-command (concat "open -a 'Google Chrome.app' file://" buffer-file-name)))
 (shell-command (concat "open -a 'Google Chrome.app' " buffer-file-name)))

(use-package pcap-mode
  :mode
  ("\\.pcapng\\'" . pcap-mode)
  :load-path (lambda () (expand-file-name "site-elisp/pcap-mode" user-emacs-directory)))

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


