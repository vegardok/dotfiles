(set-face-attribute 'default nil :family "Ubuntu Mono")
(set-face-attribute 'default nil :height 210)

(load-file "~/repos/dotfiles/sensible-defaults.el")

(sensible-defaults/use-all-settings)
(sensible-defaults/bind-commenting-and-uncommenting)

(setq init-dir "~/.emacs.d/")



(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))


;; Add marmalade to package repos
(setq package-archives nil)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)


(unless (and (file-exists-p (concat init-dir "elpa/archives/gnu"))
             (file-exists-p (concat init-dir "elpa/archives/melpa")))
  (package-refresh-contents))

 ;; (defun packages-install (&rest packages)
 ;;   (message "running packages-install")
 ;;   (mapc (lambda (package)
 ;;           (let ((name (car package))
 ;;                 (repo (cdr package)))
 ;;             (when (not (package-installed-p name))
 ;;               (let ((package-archives (list repo)))
 ;;                 (package-initialize)
 ;;                 (package-install name)))))
 ;;         packages)
 ;;   (package-initialize)
 ;;   (delete-other-windows))

 ;; (defun init--install-packages ()
 ;;   (message "Lets install some packages")
 ;;   (packages-install
 ;;    ;; Since use-package this is the only entry here
 ;;    ;; ALWAYS try to use use-package!
 ;;    (cons 'use-package melpa)))


;; Install everything
;; (condition-case nil
;;     (init--install-packages)
;;   (error
;;    (package-refresh-contents)
;;    (init--install-packages)))



;; Functions
;; (defun eslint-fix-file ()
;;   (interactive)
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory)
;;                 "node_modules"))
;;          (eslint (and root
;;                       (expand-file-name "node_modules/eslint/bin/eslint.js"
;;                                         root))))
;;     (message "eslint --fixing the file" (buffer-file-name))
;;     (shell-command (concat eslint " --fix " (buffer-file-name)))
;;     (revert-buffer t t)))


;;; Global Settings
(defalias 'list-buffers 'ibuffer)
(load-theme 'deeper-blue)
(if (not window-system)
    (set-face-background 'default "unspecified-bg"))
(set-face-foreground 'line-number-current-line "spring green")
(hl-line-mode 1)
(set-face-background 'hl-line "#2d3b42")
(global-hl-line-mode)
(blink-cursor-mode 0)
(menu-bar-mode 1)
(tool-bar-mode -1)

(setq frame-title-format "Emacs")

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)
(setq isearch-lax-whitespace nil)


(use-package company
  :ensure t
  :diminish company-mode
  :bind (("M-RET" . company-complete))
  :config
  (setq
   company-minimum-prefix-length 1
   company-idle-delay nil
   company-global-modes '(not shell-mode)))

(add-hook 'after-init-hook 'global-company-mode)

(use-package eglot
  :ensure t
  :config
  (setq
   eglot-confirm-server-initiated-edits nil
   eglot-connect-timeout 1
   eglot-events-buffer-size 0
   eglot-connect-timeout 100 ;; kotlin-language-server is slow to start
   eglot-ignored-server-capabilities '())
  :bind (("C-M-<return>" . eglot-code-actions)))

(use-package agent-shell
  :ensure t
  :ensure-system-package
  ;; Add agent installation configs here
  ((claude . "brew install claude-code")
   (claude-code-acp . "npm install -g @zed-industries/claude-code-acp"))
  :config
  (setq agent-shell-anthropic-authentication
	(agent-shell-anthropic-make-authentication :login t))
  (setq agent-shell-google-authentication
	(agent-shell-google-make-authentication :vertex-ai t))
  (setq agent-shell-google-gemini-environment
	(agent-shell-make-environment-variables
	 "GOOGLE_CLOUD_LOCATION" "global"
	 "GOOGLE_CLOUD_PROJECT" "arcline-playground")))

;; (use-package diminish
;;   :ensure t)



;; (add-hook 'python-mode-hook 'company-mode)



;; scroll one line at a time (less "jumpy" than defaults)
(setq
 mouse-wheel-scroll-amount '(2 ((shift) . 2)) ;; one line at a time
 mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
 mouse-wheel-follow-mouse 't ) ;; scroll window under mouse
(setq
 scroll-step 1
 scroll-conservatively 10000) ;; keyboard scroll one line at a time

(setq-default line-spacing 0.2)


;; Packages

(use-package try
  :ensure t)

;; (use-package dockerfile-mode
;;   :ensure t)



;; ;; status bar thing
;; (use-package powerline
;;   :ensure t
;;   :config
;;   (powerline-default-theme)
;;   (setq powerline-display-hud nil
;;         powerline-display-mule-info nil
;;         powerline-display-buffer-size nil))



;; (use-package which-key
;;   :ensure t
;;   :diminish which-key-mode
;;   :config
;;   (which-key-mode))

;; (use-package graphql-mode
;;   :ensure t)

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (projectile-mode +1)
  :config
  (setq
   projectile-use-git-grep t))
(add-hook 'compilation-filter-hook (lambda () (ansi-color-apply-on-region compilation-filter-start (point))))
(setq compilation-scroll-output t)

(use-package helm-projectile
  :ensure t
  :bind
  ("M-r" . helm-projectile-grep)
  ("C-x x" . helm-projectile))

(use-package helm
  :ensure t
  ;; :diminish helm-mode
  :bind (("C-x M-x" . execute-extended-command)
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . 'helm-mini))
  :diminish helm-mode
  :config
  (setq
   helm-candidate-number-limit 50
   helm-mode-fuzzy-match t
   helm-completion-style 'helm-fuzzy
   helm-recentf-fuzzy-match t
   helm-grep-file-path-style 'relative
   completion-styles '(flex))
  (helm-mode 1)
  (when (boundp 'helm-file-cache)
    (setq helm-file-cache nil)))

;; ;; helm-projectile-sources-list
;; (use-package helm-swoop
;;   :ensure t
;;   :bind (
;;          ("C-M-s" . helm-swoop)
;;          ;; :map prog-mode-map (("C-M-s" . helm-swoop))
;;          :map helm-swoop-map
;;          ("C-r" . helm-previous-line)
;;          ("C-s" . helm-next-line))
;;   :config
;;   (setq helm-swoop-pre-input-function (lambda () "")))

(use-package wgrep
  :ensure t
  :config (use-package wgrep-helm :ensure t))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "M-C-n") 'mc/mark-next-like-this)
  (global-set-key (kbd "M-C-p") 'mc/mark-previous-like-this))

(use-package groovy-mode
  :ensure t
  :mode "Jenkinsfile"
  :config
  (setq groovy-indent-offset 2))


(use-package flycheck
  :ensure t
  ;; :diminish flycheck-mode
  :config
  (setq flycheck-check-syntax-automatically (quote (save mode-enabled)))
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  )

(windmove-default-keybindings 'meta)

(global-unset-key "\C-t") ;; transpose-chars
(global-unset-key "\M-t") ;; transpose-words
(global-unset-key "\C-x\m") ;; mail

(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z")

;; (global-set-key (kbd "C-x c") 'comment-or-uncomment-region)

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))


;; (setq treesit-language-source-alist
;;    '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;      (cmake "https://github.com/uyha/tree-sitter-cmake")
;;      (css "https://github.com/tree-sitter/tree-sitter-css")
;;      (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;      (go "https://github.com/tree-sitter/tree-sitter-go")
;;      (html "https://github.com/tree-sitter/tree-sitter-html")
;;      (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;      (json "https://github.com/tree-sitter/tree-sitter-json")
;;      (make "https://github.com/alemuller/tree-sitter-make")
;;      (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;      (python "https://github.com/tree-sitter/tree-sitter-python")
;;      (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;      (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;      (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;      (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(use-package prettier-js
  :ensure t
  :config
  (setq
   prettier-js-command "/Users/vegardok/.nvm/versions/node/v20.18.1/bin/prettier"
   ;; prettier-js-args '("--plugin prettier-plugin-svelte")
   ))


(use-package typescript-mode
  :ensure t
  ;; :mode "\\.tsx?\\'"
  :after (flycheck)
  :config
  ;; (add-hook 'typescript-mode-hook 'eglot-ensure)
  ;; (add-hook 'typescript-ts-mode-hook 'eglot-ensure)
  (add-hook 'tsx-ts-mode-hook 'eglot-ensure)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-ts-mode-hook 'prettier-js-mode)

  ;; (define-derived-mode typescript-tsx-mode typescript-mode "TSX")
  ;; (add-to-list 'auto-mode-alist `(,(rx ".tsx" eos) . typescript-tsx-mode))
  :config
  ;; (add-hook 'typescript-mode-hook 'company-mode)
  ;; (add-hook 'typescript-tsx-mode-hook #'sgml-electric-tag-pair-mode)
  ;; (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  ;; (flycheck-add-next-checker 'lsp 'javascript-eslint 'append)
  (setq typescript-indent-level 2)

  (define-derived-mode tsx-mode typescript-mode
    "TypeScript[TSX]")
  (put 'tsx-mode 'eglot-language-id "typescriptreact")

  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-mode)))










;; (with-eval-after-load 'eglot
;;   (define-key eglot-mode-map (kbd "C-M-<return>") 'eglot-code-actions)
;;   (add-to-list 'eglot-server-programs
;;                '(svelte-mode . ("svelteserver" "--stdio"))))

;; (use-package svelte-mode
;;   :config
;;   ;; (add-hook 'svelte-mode-hook 'company-mode)
;;   (add-hook 'svelte-mode-hook 'eglot-ensure)
;;   (add-hook 'svelte-mode-hook 'prettier-js-mode)
;;   :ensure t)

;; (use-package jsonnet-mode
;;   :ensure t)


;; EDiff
;; (defvar my-ediff-last-windows nil)
;; (defun my-store-pre-ediff-winconfig ()
;;   (setq my-ediff-last-windows (current-window-configuration)))
;; (defun my-restore-pre-ediff-winconfig ()
;;   (set-window-configuration my-ediff-last-windows))

;; (add-hook 'ediff-before-setup-hook #'my-store-pre-ediff-winconfig)
;; (add-hook 'ediff-quit-hook #'my-restore-pre-ediff-winconfig)

;; ;; Markdown
(use-package markdown-mode
  :ensure t
  :bind (("<backspace>" . backward-delete-char-untabify)))

;; ;; progn hook?
;; ;; (add-hook 'emacs-lisp-mode-hook 'company-mode)

(use-package magit
  :ensure t
  ;; :diminish (magit-auto-revert-mode
  ;;      auto-revert-mode)
  :config
  ;; (add-hook 'git-commit-mode-hook 'turn-on-flyspell)
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-worktrees 'magit-insert-status-headers t)
  (setq
   magit-diff-refine-hunk 'all
   ;; magit-commit-arguments (quote ("--no-verify"))
   magit-commit-show-diff nil
   ;; magit-rebase-arguments (quote ("--autosquash"))
   ;; magit-refs-sections-hook (quote (magit-insert-local-branches))
   ;; magit-refs-show-margin nil
   ;; magit-revert-buffers nil
   ;; magit-auto-revert-mode nil
   ;; magit-visit-ref-behavior '(create-branch checkout-branch)
   ;; magit-visit-ref-behavior (quote (checkout-branch))
   magit-log-margin '(t "%Y-%m-%d" magit-log-margin-width t 18)
   magit-log-margin-show-committer-date nil
   magit-display-buffer-function (quote magit-display-buffer-same-window-except-diff-v1)
   magit-diff-highlight-indentation nil))
;; (use-package magit-delta
;;   :ensure t
;;   :hook (magit-mode . magit-delta-mode))


;; (use-package auto-dim-other-buffers
;;   :ensure t
;;   :config
;;   (setq auto-dim-other-buffers-mode t
;;         auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
;;   (custom-set-faces
;;    '(auto-dim-other-buffers-face ((t (:background "gray25"))))))

;; helm-m-x is currently used
;; (use-package counsel
;; :ensure t
;; :bind (("M-x" . counsel-M-x))
;; )


;; (use-package restclient
;;   :ensure t)

(use-package lorem-ipsum
  :ensure t)

;; (use-package yaml-mode
;;   :ensure t)

;; (use-package terraform-mode
;;   :ensure t)

;; (use-package rust-mode
;;   :ensure t
;;   :mode "\\.rs\\'"
;;   :init

;;   ;; (add-hook 'rust-mode-hook 'company-mode)
;;   :bind
;;   ("C-c C-c" . rust-run)
;;   :config
;;   (setq rust-format-on-save nil))

;; (use-package cargo
;;   :ensure t
;;   :hook ((rust-mode toml-mode) . cargo-minor-mode))

;; (use-package toml-mode
;;   :mode "\\.toml\\'"
;;   :ensure t)

(use-package uuidgen
  :ensure t)

;; (use-package nginx-mode
;;   :ensure t)

;; (use-package editorconfig
;;   :ensure t
;;   :diminish editorconfig-mode
;;   :config
;;   (editorconfig-mode 1))

;; (use-package kotlin-mode
;;   :ensure t)


;; (package-install-file "~/tmp/gptel")


;; Org-mode
(use-package org
  :ensure t
  :pin gnu
  :config
  (setq org-todo-keyword-faces
        '(("TODO" . "#cc3342")      ; Set the TODO state color
          ("WIP"  . "#e3a51c")   ; Set the WIP state color
          ("DONE" . "#3bc455"))))  ; Set the DONE state color

(with-eval-after-load 'org
  ;; Highlight src blocks using the major mode mapped here
  (add-to-list 'org-src-lang-modes '("ts" . typescript))
)
(scroll-bar-mode -1)
(winner-mode t)



;; (load-file "~/repos/dotfiles/gptel.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agent-shell-anthropic-default-model-id "claude-opus-4-6")
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.emacs.d/saves")))
 '(css-indent-offset 2)
 '(eglot-connect-timeout 9999999)
 '(js-indent-level 2)
 '(magit-list-refs-sortby "-creatordate")
 '(package-selected-packages
   '(agent-shell company eglot exec-path-from-shell flycheck gptel graphql-mode groovy-mode helm-projectile lorem-ipsum
		 magit markdown-mode mcp multiple-cursors prettier-js try typescript-mode uuidgen wgrep-helm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-line ((t nil)))
 '(whitespace-space ((t (:foreground "unemphasizedSelectedTextBackgroundColor"))))
 '(whitespace-tab ((t (:foreground "dark red")))))



(add-to-list 'load-path "~/repos/dotfiles")
(load "llm.el")
