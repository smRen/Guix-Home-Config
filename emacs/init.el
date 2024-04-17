(use-package emacs
  :config
  ;; Set side fringes
  (set-fringe-mode 30)
  
  ;; Enable line numbers everywhere
  ;; (global-display-line-numbers-mode +1)
  
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

  ;; Always use spaces for tabs
  (indent-tabs-mode -1)

  ;; Save minibuffer history
  (savehist-mode +1)

  ;; Show symbols
  (global-prettify-symbols-mode +1)

  ;; Recent files
  (recentf-mode +1)

  ;; Associate certain files to correct ts-mode
  (add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-ts-mode))
  (add-to-list 'auto-mode-alist '("^\\.bashrc$" . bash-ts-mode))
  
  ;; Third party packages
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  ;; Make async shell command faster
  (setq read-process-output-max (* 64 1024 1024))
  (setq process-adaptive-read-buffering nil)
  (setq process-connection-type nil)
  ;; (let ((process-connection-type nil))
  ;;   (async-shell-command command buffer))

  :hook
  ;; Enable eglot in the following modes
  ;; ((c++-ts-mode bash-ts-mode) .  eglot-ensure)

  ;; Enable line modes in programming modes
  (prog-mode . display-line-numbers-mode)
  
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-setup . cursor-intangible-mode)

  :custom
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Use short answers
  (use-short-answers t)

  ;; Backup settings
  (backup-directory-alist
   `((".*" . ,(concat user-emacs-directory "backups"))))
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

  ;; ;; Completion related settings
  ;; (completion-auto-help 'always)
  ;; (completion-styles '(flex partial-completion substring))
  ;; (completion-category-overrides '((file (styles basic substring))))
  ;; (read-buffer-completion-ignore-case t)
  ;; (completions-format 'one-column)
  ;; (completion-auto-select 'second-tab)
  ;; (completions-detailed t)
  )

(use-package ansi-color
  :config
  (defun my/ansi-colorize-buffer ()
    (let ((buffer-read-only nil))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer))

(use-package elec-pair
  :hook
  ;; Enable electric pairs in following modes
  ((c++-ts-mode bash-ts-mode emacs-lisp-mode typescript-ts-mode) . electric-pair-local-mode))

;; Custom settings for C/C++
(use-package c-ts-mode
  :custom
  ;; Indent and code style
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style 'linux))

;; Custom settings for json-ts
(use-package json-ts-mode
  :custom
  (json-ts-mode-indent-offset 8))

;; ;; Default minibuffer completion
;; (use-package icomplete
;;   :demand t
;;   :custom
;;   (completion-styles '(partial-completion substring flex))
;;   (completion-category-overrides '((file (styles basic substring))))
;;   ;;  (read-file-name-completion-ignore-case t)
;;   (read-buffer-completion-ignore-case t)
;;   ;;  (completion-ignore-case t)
;;   :config
;;   ;;  (icomplete-mode)
;;   ;;  (icomplete-vertical-mode)
;;   (fido-vertical-mode +1)
;;   :bind (:map icomplete-minibuffer-map
;;               ("C-n" . icomplete-forward-completions)
;;               ("C-p" . icomplete-backward-completions)))

;;;;;;; Third party packages ;;;;;;;

;; ;; Color theme
(use-package doom-themes
  :ensure t
  :custom
  (doom-vibrant-brighter-comments t)
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-vibrant-brighter-comments t)
  (doom-vibrant-brighter-modeline t)
  (doom-vibrant-padded-modeline t)
  :config
  (load-theme 'doom-vibrant t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; LSP Mode
(use-package lsp-mode
  :ensure t
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
  :hook (;; Auto start in the following modes
	 ((c++-ts-mode bash-ts-mode cmake-ts-mode json-ts-mode typescript-ts-mode) . lsp-deferred))
  :commands (lsp lsp-deferred)
  :custom
  (lsp-idle-delay 0.1)
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 1024 1024)))

(use-package dap-mode
  :ensure t
  :hook ((dap-stopped) . (lambda (arg) (call-interactively #'dap-hydra)))
  :config
  (dap-auto-configure-mode +1)
  (require 'dap-cpptools)
  (require 'dap-node))

;; Extra lsp features
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Better terminal
(use-package vterm
  :ensure t
  :defer t)

;; Markdown
(use-package markdown-mode
  :ensure t
  :defer t)

;; Snippets
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  :hook
  ;; Enable yas-snippets in the following modes
  ((c++-ts-mode bash-ts-mode emacs-lisp-mode cmake-ts-mode json-ts-mode typescript-ts-mode) . yas-minor-mode))

;; Actual snippets
(use-package yasnippet-snippets
  :ensure t)

;; Inline completion
(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)
  :hook
  (after-init . global-company-mode))

;; Example configuration for Consult
(use-package consult
  :ensure t
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
	 ("M-g f" . consult-flycheck)
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
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  :custom
  (consult-narrow-key "<")
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode +1))
  ;;:hook (;; Set multiple checkers for flycheck
	 ;; Note: Check the LSP for integration
	 ;; May not be necessary
	 ;; (lsp-managed-mode . (lambda ()
	 ;; 		       (flycheck-mode)
	 ;; 		       (let ((current-prog-mode major-mode))
	 ;; 			 (cond ((eq current-prog-mode 'c++-ts-mode)
	 ;; 				(setq flycheck-checker 'c/c++-cppcheck)
	 ;; 				(flycheck-add-next-checker 'c/c++-cppcheck 'lsp))
	 ;; 			       ;; ((eq current-prog-mode 'bash-ts-mode)
	 ;; 			       ;; 	(setq flycheck-checker 'sh-shellcheck)
	 ;; 			       ;; 	(flycheck-add-next-checker 'sh-shellcheck 'lsp))
	 ;; 			       ))))
;;	 ))

(use-package consult-flycheck
  :ensure t)

(use-package consult-lsp
  :ensure t
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

;; Centered window
(use-package writeroom-mode
  :ensure t
  :defer t)

;; ;; Automated treesitter
(use-package treesit-auto
  :ensure t
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  :custom
  (treesit-font-lock-level 4))

;; Autofind python env
(use-package pet
  :ensure t
  :defer t
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

;; Minibuffer Completion
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (enable-recursive-minibuffers t)
  (read-buffer-completion-ignore-case t)
  (completion-styles '(orderless flex basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Move minibuffer stuff to middle
;; Very glitchy
(use-package vertico-posframe
  :ensure t
  :config
  (vertico-posframe-mode 1))

;; Completion style
(use-package orderless
  :ensure t)

;; Annotations
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode +1))

;; Icons
(use-package all-the-icons-completion
  :ensure t
  :init
  (all-the-icons-completion-mode +1)
  :hook
  (marginalia-mode . all-the-icons-completion-marginalia-setup))

;; Faster lsp
(use-package eglot-booster
  :after eglot
  :config
  (eglot-booster-mode))

;; Project management
(use-package projectile
  :ensure t
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
  :ensure t
  :bind (:map projectile-mode-map
	      ([remap projectile-find-file] . consult-projectile-find-file)
	      ([remap projectile-find-dir] . consult-projectile-find-dir)
	      ([remap projectile-find-file-other-window] . consult-projectile-find-file-other-window)
	      ([remap projectile-switch-to-buffer-other-window] . consult-projectile-switch-to-buffer-other-window)
	      ([remap projectile-recentf] . consult-projectile-recentf)
	      ([remap projectile-switch-to-buffer] . consult-projectile-switch-to-buffer))
  :after projectile)

(use-package ripgrep
  :ensure t
  :defer t)

(use-package git-modes
  :ensure t)

(use-package golden-ratio-scroll-screen
  :ensure t
  :bind
  (([remap scroll-down-command] . golden-ratio-scroll-screen-down)
   ([remap scroll-up-command] . golden-ratio-scroll-screen-up)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(golden-ratio-scroll-screen dap-mode vertico-posframe git-modes ripgrep fancy-compilation yasnippet-snippets writeroom-mode vterm vertico treesit-auto projectile pet orderless marginalia lsp-ui eglot-booster doom-themes consult-lsp consult-flycheck company all-the-icons-completion))
 '(safe-local-variable-values
   '((projectile-project-run-cmd . "npm run start")
     (projectile-project-compilation-cmd . "npm run compile"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
