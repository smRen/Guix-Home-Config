;;; Emacs-config-file --- Summary  -*- lexical-binding:t -*-

;;; Commentary:
;; Init file for Emacs

;;; Code:
(use-package emacs
  :config
  ;; Straight package manager
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
  (straight-use-package 'use-package)

  ;; Desktop mode
  (unless (daemonp)
    (desktop-save-mode +1))

  ;; Default theme
  (load-theme 'modus-vivendi t)
    
  ;; Disable menu bar, tool bar, and scroll bar
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; Disable blinking cursor
  (blink-cursor-mode -1)

  ;; Save cursor place
  (save-place-mode +1)

  ;; Pixel precision for mouse scrolling
  (pixel-scroll-precision-mode +1)

  ;; Show column number
  (column-number-mode +1)

  ;; Save minibuffer history
  (savehist-mode +1)

  ;; Show symbols
  (global-prettify-symbols-mode +1)

  ;; Recent files
  (recentf-mode +1)

  ;; Associate certain files to correct ts-mode
  (add-to-list 'auto-mode-alist '("/CMakeLists\\.txt" . cmake-ts-mode))
  (add-to-list 'auto-mode-alist '("/\\.bashrc\\'" . bash-ts-mode))
  (add-to-list 'auto-mode-alist '("/Dockerfile[[:alpha:][:digit:].]*\\'" . dockerfile-ts-mode))
  (add-to-list 'auto-mode-alist '("/Containerfile[[:alpha:][:digit:].]*\\'" . dockerfile-ts-mode))

  ;; Third party packages
  (defvar package-archives)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

    ;; Set fonts
  (add-to-list 'default-frame-alist
	       '(font . "Hack Nerd Font-11"))
  (set-face-attribute 'default t :font "Hack Nerd Font-11")

  ;; Start emacs maximized
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Custom settings in its own file
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (load custom-file 'noerror)

  ;;; Prevent Extraneous Tabs
  (setq-default indent-tabs-mode nil)

  :hook

  ;; Enable line modes in programming modes
  (prog-mode . display-line-numbers-mode)

  ;; Enable line modes in specific modes
  ((Info-mode org-mode) . display-line-numbers-mode)

  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-setup . cursor-intangible-mode)

  :custom
  ;; Allow pin entry inside of emacs minibuffer
  (epg-pinentry-mode 'loopback)
  
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; Use short answers
  (use-short-answers t)

  ;; Tab first then try complete
  (tab-always-indent 'complete)

  ;; Backup settings
  (lock-file-name-transforms
	'(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/aux/\\1" t)))
  (auto-save-file-name-transforms
	'(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/aux/\\1" t)))
  (backup-directory-alist
	'((".*" . "~/.emacs.d/aux/")))
  (kept-old-versions 2)
  (kept-new-versions 6)
  (vc-make-backup-files t)
  (backup-by-copying t)
  (delete-old-versions t)
  (version-control t)

  ;; Disable startup screen and messages
  (inhibit-startup-screen t)
  (inhibit-startup-message t)
  (initial-scratch-message nil)
  (inhibit-startup-echo-area-message "smakey18")

  ;; Name and email
  (user-full-name "Ren Abelard Arriola Odion")
  (user-mail-address "smakey18@gmail.com")

  ;; Allow consecutive mark jumps (C-u C-SPC...C-SPC)
  (set-mark-command-repeat-pop t)

  ;; Don't reset cursor position when point moves offscreen
  (scroll-conservatively 101)
  (scroll-preserve-screen-position +1)

  :bind
  ;; Use hippie expand
  ([remap dabbrev-expand] . hippie-expand)
  ;; Ibuffer
  ("C-x C-b" . ibuffer-other-window)
  ;; Recent files
  ("C-x C-r" . consult-recent-file))

;; Org mode
(use-package org
  :config
  ;; Enable org mode in the following languages
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (shell . t)
        (python . t)
        (js . t)
        (ruby . t)
        (C . t)))
  :custom
  (org-directory "~/Projects/Org")
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-confirm-babel-evaluate nil)
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-list-allow-alphabetical t)
  :bind (("C-c o s" . org-store-link)
         ("C-c o c" . org-capture)
         ("C-c o a" . org-agenda)
         :map org-mode-map
         ("C-c o <RET>" . org-meta-return)))

;; ;; Native LSP
;; (use-package eglot
;;   :hook ((c++-ts-mode bash-ts-mode typescript-ts-mode) . eglot-ensure)
;;   :custom
;;   (eldoc-echo-area-use-multiline-p nil))

;; Native file checking
;; (use-package flymake
;;   :hook ((emacs-lisp-mode) . flymake-mode)
;;   :custom ((flymake-no-changes-timeout 3)))

;; Faster lsp
;; (use-package eglot-booster
;;   :after eglot
;;   :commands (eglot-booster-mode)
;;   :init
;;   (eglot-booster-mode))

;; Window history
(use-package winner
  :bind (("C-c w u" . winner-undo)
         ("C-c w r" . winner-redo))
  :config
  (winner-mode +1))

;; Window movement
(use-package windmove
  :bind (("C-c w h" . windmove-left)
         ("C-c w l" . windmove-right)
         ("C-c w k" . windmove-up)
         ("C-c w j" . windmove-down)
         ("C-c w H" . windmove-swap-states-left)
         ("C-c w L" . windmove-swap-states-right)
         ("C-c w K" . windmove-swap-states-up)
         ("C-c w J" . windmove-swap-states-down)))

;; Add color to compilation buffer
(use-package ansi-color
  :commands (ansi-color-apply-on-region)
  :config
  (defun smren/ansi-colorize-buffer ()
    (let ((buffer-read-only nil))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'smren/ansi-colorize-buffer))

(use-package project
  :bind (("C-x p F" . flymake-show-project-diagnostics)))

(use-package elec-pair
  :hook
  ;; Enable electric pairs in following modes
  ((c++-ts-mode bash-ts-mode emacs-lisp-mode typescript-ts-mode) . electric-pair-local-mode))

(use-package sh-script
  :config
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode)))

(use-package yaml-ts-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-ts-mode)))

(use-package python
  :config
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  :custom
  (python-indent-guess-indent-offset nil))

;; Custom settings for C/C++
(use-package c-ts-mode
  :custom
  ;; Indent and code style
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style 'linux)
  :config
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode)))

(use-package c++-ts-mode
  :config
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode)))

(use-package css-mode
  :config
  (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode)))

(use-package js
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode)))

(use-package typescript-ts-mode
  :config
  (setq typescript-ts-mode-indent-offset 4)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode)))

;; Custom settings for json-ts
(use-package json-ts-mode
  :custom
  (json-ts-mode-indent-offset 8))

;; Color theme
;; (use-package doom-themes
;;   :straight t
;;   :commands (doom-themes-visual-bell-config doom-themes-org-config)
;;   :custom
;;   (doom-themes-enable-bold t)
;;   (doom-themes-enable-italic t)
;;   (doom-outrun-electric-brighter-comments t)
;;   (doom-outrun-electric-padded-modeline t)
;;   (doom-outrun-electric-brighter-modeline t)
;;   :init
;;   (load-theme 'doom-outrun-electric t)
;;   (doom-themes-visual-bell-config)
;;   (doom-themes-org-config))

;; LSP Mode
(use-package lsp-mode
  :straight t
  :commands (lsp lsp-booster--advice-final-command lsp-booster--advice-json-parse)
  :init
  ;; For LSP Booster
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
	 (when (byte-code-function-p bytecode)
	   (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
			 (fboundp 'json-parse-buffer))
		  'json-parse-buffer
		'json-read)
	      :around
	      #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?) ;; for check lsp-server-present?
	       (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
	       lsp-use-plists
	       (not (functionp 'json-rpc-connection)) ;; native json-rpc
	       (executable-find "emacs-lsp-booster"))
	  (progn
	    (message "Using emacs-lsp-booster for %s!" orig-result)
	    (cons "emacs-lsp-booster" orig-result))
	orig-result)))

  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

  (setq lsp-keymap-prefix "C-c l")

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
	  '(orderless))) ;; Configure orderless

  :hook (;; Auto start in the following modes
	 ((c++-ts-mode css-ts-mode bash-ts-mode cmake-ts-mode json-ts-mode typescript-ts-mode dockerfile-ts-mode yaml-ts-mode) . lsp)
	 (lsp-completion-mode . my/lsp-mode-setup-completion))
  :custom
  (lsp-completion-provider :none) ;; For corfu
  (lsp-idle-delay 0.1)
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 1024 1024)))

(use-package dap-mode
  :straight t
  :hook ((dap-stopped) . (lambda () (call-interactively #'dap-hydra)))
  :config
  (dap-auto-configure-mode +1)
  (require 'dap-cpptools)
  (require 'dap-node))

;; Extra lsp features
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

(use-package flycheck
  :straight t
  :commands (flycheck-add-next-checker)
  :config
  (defun smren/elisp-checker ()
    (flycheck-mode)
    (flycheck-add-next-checker 'emacs-lisp 'emacs-lisp-checkdoc))
  :hook
  ((emacs-lisp-mode smren/elisp-check))
  :custom
  (flycheck-idle-change-delay 3))

;; Better terminal
(use-package vterm
  :straight t)

;; Markdown
(use-package markdown-mode
  :straight t)

;; Snippets
(use-package yasnippet
  :straight t)

;; Actual snippets
(use-package yasnippet-snippets
  :straight t)

;; Completions
(use-package corfu
  :straight t
  :commands (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  :init
  :hook ((prog-mode . corfu-mode)
	 (shell-mode . corfu-mode)
	 (eshell-mode . corfu-mode)))

;; Git porcelain
(use-package magit
  :straight t
  :bind (("C-c g" . magit-dispatch)
         ("C-c f" . magit-file-dispatch)))

;; Example configuration for Consult
(use-package consult
  :straight t
  :bind (("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
	 ("C-x t b" . consult-buffer-other-tab) ;; orig. switch-to-buffer-other-tab
	 ("C-x r b" . consult-bookmark)		;; orig. bookmark-jump
	 ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer

	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop) ;; orig. yank-pop
	 ;; M-g bindings in `goto-map'
	 ("M-g e" . consult-compile-error)
	 ("M-g g" . consult-goto-line)		  ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)	  ;; orig. goto-line
	 ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings in `search-map'
	 ("M-s d" . consult-fd) ;; Alternative: consult-fd
	 ("M-s c" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
	 ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history) ;; orig. next-matching-history-element
	 ("M-r" . consult-history)) ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  :custom
  (consult-narrow-key "<")
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package consult-flycheck
  :straight t
  :after flycheck
  :bind (("M-g f" . consult-flycheck)))

(use-package consult-lsp
  :commands (consult-lsp-symbols)
  :straight t
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

;; For downloading treesit languages
(use-package treesit-auto
  :straight t)

;; Autofind python env
(use-package pet
  :straight t
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

;; Minibuffer Completion
(use-package vertico
  :straight t
  :commands (vertico-mode)
  :init
  (vertico-mode)
  :custom
  (enable-recursive-minibuffers t)
  (read-buffer-completion-ignore-case t)
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Completion style
(use-package orderless
  :straight t)

;; Annotations
(use-package marginalia
  :straight t
  :commands (marginalia-mode)
  :init
  (marginalia-mode +1))

;; Icons
(use-package nerd-icons
  :straight t)

(use-package nerd-icons-completion
  :straight t
  :after marginalia
  :commands (nerd-icons-completion-mode nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Project management
(use-package projectile
  :straight t
  :commands (projectile-mode)
  :defines (projectile-mode-map)
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map))
  :hook
  (project-find-functions . project-projectile)
  :custom
  ;; Allow compilation buffer to be editable (useful for interactive apps)
  (projectile-comint-mode t)
  ;; Auto search Projects folder for projects
  (projectile-project-search-path '("~/Projects")))

(use-package consult-projectile
  :straight t
  :bind (:map projectile-mode-map
              ([remap projectile-switch-project] . consult-projectile-switch-project)
              ([remap projectile-find-file] . consult-projectile-find-file)
	      ([remap projectile-find-dir] . consult-projectile-find-dir)
	      ([remap projectile-find-file-other-window] . consult-projectile-find-file-other-window)
	      ([remap projectile-switch-to-buffer-other-window] . consult-projectile-switch-to-buffer-other-window)
	      ([remap projectile-recentf] . consult-projectile-recentf)
	      ([remap projectile-switch-to-buffer] . consult-projectile-switch-to-buffer))
  :after projectile)

(use-package golden-ratio-scroll-screen
  :straight t
  :bind
  (([remap scroll-down-command] . golden-ratio-scroll-screen-down)
   ([remap scroll-up-command] . golden-ratio-scroll-screen-up)))

;; (use-package doom-modeline
;;   :straight t
;;   :commands (doom-modeline-mode)
;;   :init (doom-modeline-mode 1)
;;   :custom
;;   (doom-modeline-vcs-max-length 30))

(use-package corfu-terminal
  :straight t
  :commands (corfu-terminal-mode)
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package nerd-icons-corfu
  :straight t
  :after corfu
  :defines (corfu-margin-formatters)
  :commands (nerd-icons-corfu-formatter)
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package exec-path-from-shell
  :straight t
  :config
  (dolist (var '("LANG" "SSH_AUTH_SOCK"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package inf-ruby
  :straight t)

;;; Init.el ends here
