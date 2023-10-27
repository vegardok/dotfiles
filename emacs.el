(load-file "~/repos/dotfiles/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/bind-commenting-and-uncommenting)

(setq init-dir "~/.emacs.d/")

(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar org-elpa '("org" . "http://orgmode.org/elpa/"))

;; Add marmalade to package repos
(setq package-archives nil)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)
(add-to-list 'package-archives org-elpa t)

(unless (and (file-exists-p (concat init-dir "elpa/archives/gnu"))
             (file-exists-p (concat init-dir "elpa/archives/melpa")))
  (package-refresh-contents))

(defun packages-install (&rest packages)
  (message "running packages-install")
  (mapc (lambda (package)
          (let ((name (car package))
                (repo (cdr package)))
            (when (not (package-installed-p name))
              (let ((package-archives (list repo)))
                (package-initialize)
                (package-install name)))))
        packages)
  (package-initialize)
  (delete-other-windows))

(defun init--install-packages ()
  (message "Lets install some packages")
  (packages-install
   ;; Since use-package this is the only entry here
   ;; ALWAYS try to use use-package!
   (cons 'use-package melpa)))


;; Install everything
(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))



;; Functions
(defun eslint-fix-file ()
  (interactive)
  (let* ((root (locate-dominating-file
		(or (buffer-file-name) default-directory)
		"node_modules"))
	 (eslint (and root
		      (expand-file-name "node_modules/eslint/bin/eslint.js"
					root))))
    (message "eslint --fixing the file" (buffer-file-name))
    (shell-command (concat eslint " --fix " (buffer-file-name)))
    (revert-buffer t t)))


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
(menu-bar-mode -1)

(setq frame-title-format "Emacs")

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)
(setq isearch-lax-whitespace nil)


(use-package company
  :ensure t
  ;; :diminish company-mode
  :bind (("M-RET" . company-complete))
  :config
  (setq
   company-minimum-prefix-length 1
   company-idle-delay 0
	 company-global-modes '(not shell-mode)))
(add-hook 'after-init-hook 'global-company-mode)

(use-package eglot
	:ensure t
	:config
	(setq
	 eglot-confirm-server-initiated-edits nil
	 eglot-events-buffer-size 0)
	:bind (("C-M-<return>" . eglot-code-actions)))

(use-package diminish
  :ensure t)



;; (add-hook 'python-mode-hook 'company-mode)



;; scroll one line at a time (less "jumpy" than defaults)
(setq
 mouse-wheel-scroll-amount '(2 ((shift) . 2)) ;; one line at a time
 mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
 mouse-wheel-follow-mouse 't ) ;; scroll window under mouse
(setq
 scroll-step 1
 scroll-conservatively 10000) ;; keyboard scroll one line at a time
(set-face-attribute 'default nil :family "Ubuntu Mono")
(set-face-attribute 'default nil :height 150)
(setq-default line-spacing 0.2)


;; Packages

(use-package try
  :ensure t)

(use-package dockerfile-mode
  :ensure t)



;; status bar thing
(use-package powerline
  :ensure t
  :config (powerline-default-theme))


(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package graphql-mode
  :ensure t)

(use-package projectile
  :ensure t
	 :init
  (projectile-mode +1)
  :config
  (setq
   projectile-use-git-grep t
   projectile-project-search-path '("~/repos" ("~/repos/omny-frontend" . 2))))

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
  :config
  (setq
   helm-candidate-number-limit 50
   helm-mode-fuzzy-match t
   helm-completion-style 'helm-fuzzy
   helm-recentf-fuzzy-match t
   helm-grep-file-path-style 'relative
   completion-styles '(flex))
  (helm-mode 1))

;; helm-projectile-sources-list
(use-package helm-swoop
  :ensure t
  :bind (
	 ()
	 :map prog-mode-map (("C-M-s" . helm-swoop))
	 :map helm-swoop-map
	 ("C-r" . helm-previous-line)
	 ("C-s" . helm-next-line))
  :config
  (setq helm-swoop-pre-input-function (lambda () "")))

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


(use-package nodejs-repl
  :ensure t)


(use-package json-mode :ensure t)
(use-package nodejs-repl :ensure t)


(use-package treesit
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '(
							 (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
							 (elisp "https://github.com/Wilfred/tree-sitter-elisp")
							 (go "https://github.com/tree-sitter/tree-sitter-go")
							 (html "https://github.com/tree-sitter/tree-sitter-html")
							 (json "https://github.com/tree-sitter/tree-sitter-json")
							 (make "https://github.com/alemuller/tree-sitter-make")
							 (markdown "https://github.com/ikatyang/tree-sitter-markdown")
							 (toml "https://github.com/tree-sitter/tree-sitter-toml")
							 (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
               (css "https://github.com/tree-sitter/tree-sitter-css")
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
               (python "https://github.com/tree-sitter/tree-sitter-python")
							 (svelte "https://github.com/Himujjal/tree-sitter-svelte" "master" "src")
               ;; (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))

               (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping '((python-mode . python-ts-mode)
                     (css-mode . css-ts-mode)
                     ;; (typescript-mode . tsx-ts-mode)
                     (json-mode . json-ts-mode)
                     (js-mode . js-ts-mode)
                     (css-mode . css-ts-mode)
                     (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  :config
  (mp-setup-install-grammars)
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  ;; (use-package combobulate
  ;;   :preface
  ;;   ;; You can customize Combobulate's key prefix here.
  ;;   ;; Note that you may have to restart Emacs for this to take effect!
  ;;   (setq combobulate-key-prefix "C-c o")

    ;; Optional, but recommended.
    ;;
    ;; You can manually enable Combobulate with `M-x
    ;; combobulate-mode'.
    ;; :hook ((python-ts-mode . combobulate-mode)
    ;;       (js-ts-mode . combobulate-mode)
    ;;       (css-ts-mode . combobulate-mode)
    ;;       (yaml-ts-mode . combobulate-mode)
    ;;       (json-ts-mode . combobulate-mode)
    ;;       (typescript-ts-mode . combobulate-mode)
    ;;       (tsx-ts-mode . combobulate-mode))
    ;; Amend this to the directory where you keep Combobulate's source
    ;; code.
  ;; :load-path ("~/tmp/combobulate"))
)

(use-package sgml-mode
  :hook
  (html-mode . (lambda () (setq me/pretty-print-function #'sgml-pretty-print)))
  ;; (html-mode . sgml-electric-tag-pair-mode)
  ;; (html-mode . sgml-name-8bit-mode)
  :custom
  (sgml-basic-offset 2))

(use-package prettier-js
  :ensure t
  :config
  (setq
   prettier-js-command "/Users/vegardok/repos/omny-frontend/node_modules/.bin/prettier"
   prettier-js-args '("--plugin prettier-plugin-svelte")))



(use-package typescript-mode
  :ensure t
  ;; :mode "\\.tsx?\\'"
  :after (flycheck)
  :config
  (add-hook 'typescript-mode-hook 'eglot-ensure)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  ;; (define-derived-mode typescript-tsx-mode typescript-mode "TSX")
  ;; (add-to-list 'auto-mode-alist `(,(rx ".tsx" eos) . typescript-tsx-mode))
  :config
  ;; (add-hook 'typescript-mode-hook 'company-mode)
  ;; (add-hook 'typescript-tsx-mode-hook #'sgml-electric-tag-pair-mode)
  ;; (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  ;; (flycheck-add-next-checker 'lsp 'javascript-eslint 'append)
  (setq typescript-indent-level 2))




(with-eval-after-load 'eglot
	(define-key eglot-mode-map (kbd "C-M-<return>") 'eglot-code-actions)
  (add-to-list 'eglot-server-programs
							 '(svelte-mode . ("svelteserver" "--stdio"))))

(use-package svelte-mode
  :config
  ;; (add-hook 'svelte-mode-hook 'company-mode)
  (add-hook 'svelte-mode-hook 'eglot-ensure)
  (add-hook 'svelte-mode-hook 'prettier-js-mode)
  :ensure t)

(use-package jsonnet-mode
  :ensure t)


;; EDiff
(defvar my-ediff-last-windows nil)
(defun my-store-pre-ediff-winconfig ()
  (setq my-ediff-last-windows (current-window-configuration)))
(defun my-restore-pre-ediff-winconfig ()
  (set-window-configuration my-ediff-last-windows))

(add-hook 'ediff-before-setup-hook #'my-store-pre-ediff-winconfig)
(add-hook 'ediff-quit-hook #'my-restore-pre-ediff-winconfig)

;; Markdown
(use-package markdown-mode
  :ensure t
  :bind (("<backspace>" . backward-delete-char-untabify)))

;; progn hook?
;; (add-hook 'emacs-lisp-mode-hook 'company-mode)

(use-package magit
  :ensure t
  ;; :diminish (magit-auto-revert-mode
	;;      auto-revert-mode)
  :config
  (add-hook 'git-commit-mode-hook 'turn-on-flyspell)
  (setq
   magit-diff-refine-hunk 'all
   ;; magit-commit-arguments (quote ("--no-verify"))
   magit-commit-show-diff nil
   ;; magit-rebase-arguments (quote ("--autosquash"))
   magit-refs-sections-hook (quote (magit-insert-local-branches))
   ;; magit-refs-show-margin nil
   ;; magit-revert-buffers nil
   ;; magit-auto-revert-mode nil
   ;; magit-visit-ref-behavior '(create-branch checkout-branch)
   ;; magit-visit-ref-behavior (quote (checkout-branch))
   magit-log-margin '(t "%Y-%m-%d" magit-log-margin-width t 18)
   magit-log-margin-show-committer-date nil
   magit-display-buffer-function (quote magit-display-buffer-same-window-except-diff-v1)
   ))


(use-package auto-dim-other-buffers
  :ensure t
  :config
  (setq auto-dim-other-buffers-mode t
	auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
  (custom-set-faces
   '(auto-dim-other-buffers-face ((t (:background "gray25"))))))

;; helm-m-x is currently used
;; (use-package counsel
;; :ensure t
;; :bind (("M-x" . counsel-M-x))
;; )


(use-package restclient
  :ensure t)

(use-package lorem-ipsum
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init

  ;; (add-hook 'rust-mode-hook 'company-mode)
  :bind
  ("C-c C-c" . rust-run)
  :config
  (setq rust-format-on-save nil))

(use-package cargo
  :ensure t
  :hook ((rust-mode toml-mode) . cargo-minor-mode))

(use-package toml-mode
  :mode "\\.toml\\'"
  :ensure t)

(use-package uuidgen
  :ensure t)

(use-package nginx-mode
  :ensure t)


;; (desktop-read)
;; (desktop-save-mode 1)


;; Customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
	 [default default default italic underline success warning error])
 '(ansi-color-names-vector
	 ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(auto-dim-other-buffers-mode t)
 '(background-mode dark)
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.emacs.d/saves")))
 '(blink-cursor-mode nil)
 '(calendar-week-start-day 1)
 '(company-dabbrev-ignore-case 'keep-prefix)
 '(create-lockfiles nil)
 '(css-indent-offset 2)
 '(cursor-color "#cccccc")
 '(custom-enabled-themes '(deeper-blue))
 '(delete-old-versions t)
 '(explicit-bash-args '("--noediting" "--login" "-i"))
 '(fill-column 100)
 '(foreground-color "#cccccc")
 '(global-auto-complete-mode nil)
 '(global-font-lock-mode t)
 '(grep-find-ignored-directories
	 '("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "dist" "node_modules" "external" "coverage" "vendor" "out" "build"))
 '(grep-find-ignored-files
	 '(".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.lock" "package-lock.json"))
 '(groovy-indent-offset 2)
 '(helm-mode t)
 '(imenu-auto-rescan t)
 '(indent-tabs-mode t)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(jsonnet-indent-level 4)
 '(kept-new-versions 2)
 '(kept-old-versions 2)
 '(magit-clone-set-remote.pushDefault t)
 '(magit-diff-highlight-indentation nil)
 '(minibuffer-prompt-properties
	 '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
 '(nginx-indent-level 2)
 '(package-selected-packages
	 '(company-mode graphql-mode svelte-mode python-mode go-mode ein pyvenv lsp-pyright nginx-mode prettier-js helm-lsp lsp-ui lsp-mode uuidgen jsonnet-mode peg eglot cargo flycheck-rust rust-mode terraform-mode flx counsel dockerfile-mode groovy-mode wgrep yaml-mode treemacs-projectile treemacs tide typescript-mode restclient smartparens cljr-helm clj-refactor lorem-ipsum cider clojure-mode auto-dim-other-buffers org-bullets org-mode helm-c-yasnippet yasnippet-snippets yasnippet powerline company-tern tern exec-path-from-shell which-key web-mode use-package try scala-mode rjsx-mode nodejs-repl multiple-cursors markdown-mode magit json-mode helm-swoop helm-projectile helm-ls-git haskell-mode flycheck diminish company-web))
 '(pixel-scroll-precision-mode t)
 '(pop-up-windows t)
 '(projectile-use-git-grep t)
 '(require-final-newline nil)
 '(ruby-deep-arglist nil)
 '(same-window-regexps '("*"))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace nil)
 '(standard-indent 2)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(truncate-partial-width-windows nil)
 '(version-control 'never)
 '(w3m-home-page "https://news.ycombinator.com")
 '(warning-suppress-types '((lsp-mode)))
 '(whitespace-style
	 '(face spaces tabs newline space-mark tab-mark newline-mark trailing indentation))
 '(winner-mode t))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:background "gray25"))))
 '(column-marker-1 ((t (:background "dark red"))))
 '(helm-buffer-process ((t (:foreground "sienna1"))))
 '(helm-ff-directory ((t (:foreground "deep sky blue"))))
 '(helm-ff-file ((t (:inherit nil :foreground "PeachPuff1"))))
 '(helm-selection ((t (:background "dark slate gray" :distant-foreground "black"))))
 '(org-ellipsis ((t (:foreground "LightGoldenrod"))))
 '(whitespace-newline ((t (:foreground "dim gray" :weight normal))))
 '(whitespace-space ((t (:foreground "#3f4554"))))
 '(whitespace-trailing ((t (:background "#FF0000" :foreground "#FFFFFF" :inverse-video nil :underline nil :slant normal :weight bold)))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


(diminish 'subword-mode)


(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode t)))

(add-hook 'prog-mode-hook (lambda ()
			    (whitespace-mode t)
			    (diminish 'whitespace-mode)
			    ))
