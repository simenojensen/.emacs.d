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

(defconst *sys/gui*
  (display-graphic-p)
  "Are we running on a GUI Emacs?")

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

(setq frame-title-format '(:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))
;; (setq-default frame-title-format (list "[" user-login-name "@" (system-name) "] %b"))   ;; Set frame title to [user@hostname]*BufferName*
(global-display-line-numbers-mode)                                                      ;; Display line numbers
(setq column-number-mode t)                                                             ;; Display column numbers
(setq-default inhibit-startup-screen t)                                                 ;; Don't show the startup message
(setq-default initial-scratch-message nil)                                              ;; Set initial scratch message to nil
(setq debug-on-error t)                                                               ;; Receive more information errors
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

(bind-key "C-x C-l" 'toggle-truncate-lines)

(bind-key "M-p" 'backward-paragraph)
(bind-key "M-n" 'forward-paragraph)
(bind-key "M-g" 'goto-line)

(bind-key "C-x b" 'ibuffer)

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
  ("C-x C-b" . ivy-switch-buffer)
  ("C-s" . swiper-isearch))
  :config
  (ivy-mode 1)
  (setq ivy-height 10)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-display-style 'fancy)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

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

(use-package snails
  :disabled
  :load-path (lambda () (expand-file-name "site-elisp/snails/" user-emacs-directory))
  :if *sys/gui*
  :custom-face
  (snails-content-buffer-face ((t (:background "#111" :height 110))))
  (snails-input-buffer-face ((t (:background "#222" :foreground "gold" :height 110))))
  (snails-header-line-face ((t (:inherit font-lock-function-name-face :underline t :height 1.1)))))

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
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))

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
         (setq exec-path-from-shell-variables  '("PATH" "MANPATH" "AIRTABLE_API_KEY" "TSI_ENVIRONMENT" "TSI_TENANT_ID" "TSI_CLIENT_ID" "TSI_CLIENT_SECRET" "TSI_APPLICATION_NAME" "VIRTUAL_ENV" "LANG" "LC_ALL" "LC_CTYPE"))
         (exec-path-from-shell-initialize))
       )
      ((eq system-type 'windows-nt)
       
       )
      ((eq system-type 'gnu/linux)
       
       ))

(use-package aweshell
  :disabled
  :load-path (lambda () (expand-file-name "site-elisp/aweshell" user-emacs-directory))
  :commands (aweshell-new aweshell-dedicated-open)
  :bind
  (("s-!" . aweshell-dedicated-open)
   (:map eshell-mode-map ("s-!" . aweshell-dedicated-close))))

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
  :custom
  (treemacs-collapse-dirs 3)
  (treemacs-deferred-git-apply-delay 0.5)
  (treemacs-display-in-side-window t)
  (treemacs-file-event-delay 5000)
  (treemacs-file-follow-delay 0.2)
  (treemacs-follow-after-init t)
  (treemacs-follow-recenter-distance 0.1)
  (treemacs-git-command-pipe "")
  (treemacs-goto-tag-strategy 'refetch-index)
  (treemacs-indentation 2)
  (treemacs-indentation-string " ")
  (treemacs-is-never-other-window nil)
  (treemacs-max-git-entries 5000)
  (treemacs-no-png-images nil)
  (treemacs-no-delete-other-windows t)
  (treemacs-project-follow-cleanup nil)
  (treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
  (treemacs-recenter-after-file-follow nil)
  (treemacs-recenter-after-tag-follow nil)
  (treemacs-show-cursor nil)
  (treemacs-show-hidden-files t)
  (treemacs-silent-filewatch nil)
  (treemacs-silent-refresh nil)
  (treemacs-sorting 'alphabetic-desc)
  (treemacs-space-between-root-nodes t)
  (treemacs-tag-follow-cleanup t)
  (treemacs-tag-follow-delay 1.5)
  (treemacs-width 50)
  :config
  :bind
  (("M-0" . treemacs-select-window)
   (:map treemacs-mode-map ("C-p" . treemacs-previous-line))
   (:map treemacs-mode-map ("C-n" . treemacs-next-line))))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :after treemacs dired
  :disabled
  :config
  (treemacs-icons-dired-mode))

(use-package flycheck
  :defer t
  :diminish
  :hook ((prog-mode markdown-mode) . flycheck-mode)
  :custom
  (flycheck-global-modes
   '(not text-mode outline-mode fundamental-mode org-mode
         diff-mode shell-mode eshell-mode term-mode))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'right-fringe)
  :init
  (use-package flycheck-grammarly :defer t)
  (use-package flycheck-posframe
    :custom-face (flycheck-posframe-border-face ((t (:inherit default))))
    :hook (flycheck-mode . flycheck-posframe-mode)
    :custom
    (flycheck-posframe-border-width 1)
    (flycheck-(point)osframe-inhibit-functions
              '((lambda (&rest _) (bound-and-true-p company-backend)))))
  (use-package flycheck-pos-tip
    :defines flycheck-pos-tip-timeout
    :hook (flycheck-mode . flycheck-pos-tip-mode)
    :custom (flycheck-pos-tip-timeout 30))
  :config
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'typescript-tslint 'rjsx-mode))

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

(use-package lsp-mode
  :defer t
  :commands lsp
  :init
  (setq lsp-keymap-prefix "s-l")
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq read-process-output-max (* 1024 1024))           ;; 1mb
  (setq lsp-idle-delay 0.5))                             ;; lsp refresh rate

(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :bind
  ;; lsp-ui-peek
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ;; lsp-ui-doc
        ("M-i" . lsp-ui-doc-focus-frame))
  :config
  ;; lsp-ui-sideline
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-update-mode "point")
  (setq lsp-ui-sideline-delay 0.2)
  ;; lsp-ui-doc
  (setq lsp-ui-doc-delay 1.0)
  (setq lsp-ui-doc-use-webkit t)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil))
  ;; lsp-ui-sideline
  :hook (python-mode . lsp-ui-sideline))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package company
  :diminish company-mode
  :hook
  ((prog-mode . company-mode)
   (latex-mode . company-mode)
   (tex-mode . company-mode))
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-require-match 'never)
  (setq company-idle-delay 0)
  (setq company-show-numbers t)
  (global-company-mode 1)
  ;; Don't use company in debugger mode
  (setq company-global-modes '(not gud-mode))
  ;; Remap company's select next/previous condidate
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)))

(use-package company-lsp
  :config
  (setq company-lsp-cache-candidates 'auto))

(use-package company-tabnine
  :defer 1
  :hook
  (lsp-after-open . (lambda ()
                      (setq company-tabnine-max-num-results 3)
                      (add-to-list 'company-transformers 'company//sort-by-tabnine t)
                      (add-to-list 'company-backends '(company-lsp :with company-tabnine :separate))))
  (kill-emacs . company-tabnine-kill-process)
  :config
  (setq company-tabnine-max-num-results 9)
  ;; Enable TabNine on default
  (add-to-list 'company-backends #'company-tabnine)

  ;; Integrate company-tabnine with lsp-mode
  (defun company//sort-by-tabnine (candidates)
    (if (or (functionp company-backend)
            (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
        candidates
      (let ((candidates-table (make-hash-table :test #'equal))
            candidates-lsp
            candidates-tabnine)
        (dolist (candidate candidates)
          (if (eq (get-text-property 0 'company-backend candidate)
                  'company-tabnine)
              (unless (gethash candidate candidates-table)
                (push candidate candidates-tabnine))
            (push candidate candidates-lsp)
            (puthash candidate t candidates-table)))
        (setq candidates-lsp (nreverse candidates-lsp))
        (setq candidates-tabnine (nreverse candidates-tabnine))
        (nconc (seq-take candidates-tabnine 3)
               (seq-take candidates-lsp 6))))))

(use-package lsp-python-ms
  :diminish
  :init
  (setq lsp-python-ms-auto-install-server t)
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-python-ms)
                   (lsp)))
  :config
  (setq lsp-python-ms-executable
      "~/.emacs.d/site-elisp/python-language-server/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer"))

(use-package conda
  :init
  (setq conda-anaconda-home "/opt/miniconda3/")
  :config
  (conda-env-initialize-interactive-shells) ;; interactive shell support
  (conda-env-initialize-eshell)             ;; eshell support
  (conda-env-autoactivate-mode t))          ;; autoactivate

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
  (cursor ((t (:background "BlanchedAlmond"))))
  :config
  (doom-themes-visual-bell-config)  ;; flashing mode-line on errors
  (doom-themes-org-config)          ;; Corrects (and improves) org-mode's native fontification.
  (load-theme 'doom-gruvbox t))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq inhibit-compacting-font-caches t)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-height 15))

;;(use-package solarized-theme)
;;(use-package darktooth-theme)
;;(use-package kaolin-themes)
;; (use-package gruvbox-theme
  ;; :config
  ;; (load-theme 'gruvbox))

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

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "C-r") 'isearch-backward)
  (add-hook 'pdf-view-mode-hook (lambda ()
                                  (bms/pdf-midnite-amber)))) ; automatically turns on midnight-mode for pdfs

(use-package auctex-latexmk
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package reftex
  :diminish
  :config
  (setq reftex-cite-prompt-optional-args t)) ;; Prompt for empty optional arguments in cite

(use-package company-auctex
  :init
  (company-auctex-init))

(use-package cdlatex)

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config (progn
            (setq TeX-source-correlate-mode t)
            (setq TeX-source-correlate-method 'synctex)
            (setq TeX-auto-save t)
            (setq TeX-parse-self t)
            (setq-default TeX-master nil)
            (setq reftex-plug-into-AUCTeX t)
            (pdf-tools-install)
            (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
                  TeX-source-correlate-start-server t)
            ;; Update PDF buffers after successful LaTeX runs
            (add-hook 'TeX-after-compilation-finished-functions
                      #'TeX-revert-document-buffer)
            (add-hook 'LaTeX-mode-hook
                      (lambda ()
                        (reftex-mode t)))))

(use-package org
  :ensure org-plus-contrib
  :pin org
  :hook
  ((after-save . my/tangle-emacs-config)
   (org-mode . turn-on-org-cdlatex))
  :config
  ;; Tangle on saving this file
  (defun my/tangle-emacs-config ()
    "If the current file is this file, the code blocks are tangled"
    (when (equal (buffer-file-name) (expand-file-name "~/.emacs.d/my-literate-emacs-configuration.org"))
      (org-babel-tangle nil "~/.emacs.d/init.el")))
  (setq org-special-ctrl-a/e t)
  (setq org-src-window-setup 'split-window-below)
  ;; ;; Better Headers
  ;; (let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
  ;;                              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
  ;;                              ((x-list-fonts "Verdana")         '(:font "Verdana"))
  ;;                              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
  ;;                              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
  ;;        (base-font-color     (face-foreground 'default nil 'default))
  ;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  ;;   (custom-theme-set-faces 'user
  ;;                           `(org-level-8 ((t (,@headline ,@variable-tuple))))
  ;;                           `(org-level-7 ((t (,@headline ,@variable-tuple))))
  ;;                           `(org-level-6 ((t (,@headline ,@variable-tuple))))
  ;;                           `(org-level-5 ((t (,@headline ,@variable-tuple))))
  ;;                           `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
  ;;                           `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
  ;;                           `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
  ;;                           `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
  ;;                           `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))
  ;; ;; edit block inserts
  (setq org-structure-template-alist
  '(("a" . "export ascii\n")
    ("c" . "center\n")
    ("C" . "comment\n")
    ("e" . "src emacs-lisp\n")
    ("E" . "export")
    ("h" . "export html\n")
    ("l" . "export latex\n")
    ("q" . "quote\n")
    ("s" . "src")
    ("v" . "verse\n"))))

(use-package toc-org
  :after org
  :hook
  (org-mode . toc-org-enable))

(use-package org-bullets
  :hook
  (org-mode . (lambda () (org-bullets-mode t))))

(use-package ox-reveal
  :ensure ox-reveal
  :config
  (setq org-reveal-root "/Users/simenojensen/.emacs.d/site-elisp/reveal.js/")
  (setq org-reveal-mathjax t))

(use-package htmlize)

(defun my/edit-config ()
  "Opens the my-literate-emacs-configuration.org file."
  (interactive)
  (find-file "~/.emacs.d/my-literate-emacs-configuration.org"))

(use-package google-this
  :diminish
  :config
  (google-this-mode t))
