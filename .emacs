(setq init-dir "~/.emacs.d/")
(package-initialize)

(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))
(defvar org-elpa '("org" . "http://orgmode.org/elpa/"))

;; Add marmalade to package repos
(setq package-archives nil)
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)
(add-to-list 'package-archives org-elpa t)

(unless (and (file-exists-p (concat init-dir "elpa/archives/gnu"))
             (file-exists-p (concat init-dir "elpa/archives/melpa"))
             (file-exists-p (concat init-dir "elpa/archives/melpa-stable")))
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
   (cons 'use-package melpa)
   ))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(use-package try
  :ensure t)

(use-package diminish
  :ensure t
  :config
  (diminish 'global-whitespace-mode)
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode)
  (diminish 'tern-mode)
  )

;; Emacs UI

;;; Theme
(load-theme 'deeper-blue)
(if (not window-system)
    (set-face-background 'default "unspecified-bg"))
(set-face-foreground 'line-number-current-line "spring green")
(hl-line-mode 1)
(set-face-background 'hl-line "#2d3b42")
(global-hl-line-mode)
(blink-cursor-mode 0)
(menu-bar-mode -1)
(global-display-line-numbers-mode 1)

;;; M-x font
(cond
 ((string-equal system-type "gnu/linux")
  (progn
    (add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
    (defun my-minibuffer-setup ()
      (set (make-local-variable 'face-remapping-alist)
           '((default :height 3.0))))))
 ((string-equal system-type "darwin")
  (progn
    (menu-bar-mode)
    (add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
    (defun my-minibuffer-setup ()
      (set (make-local-variable 'face-remapping-alist)
           '((default :height 2.0 )))))))
(defalias 'list-buffers 'ibuffer)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package projectile
  :ensure t)

(use-package helm-projectile
  :ensure t
  :bind
  ("M-r" . helm-projectile-grep)
  ("C-x x" . helm-projectile))

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . 'helm-mini))
  :config
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-mini-default-sources
        '(helm-source-buffers-list
          helm-source-ls-git
          helm-source-recentf
          helm-source-buffer-not-found))
  (setq helm-grep-file-path-style 'relative)
  (helm-mode 1))

(use-package helm-ls-git :ensure t)

(use-package helm-swoop
  :ensure t
  :bind (("C-s" . helm-swoop)
         :map helm-swoop-map
         ("C-r" . helm-previous-line)
         ("C-s" . helm-next-line)
         :map helm-multi-swoop-map
         ("C-s" . helm-next-line)
         ("C-r" . helm-previous-line))
  :config
  (setq helm-swoop-pre-input-function (lambda () "")))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "M-C-<down>") 'mc/mark-next-like-this)
  (global-set-key (kbd "M-C-<up>") 'mc/mark-previous-like-this))

(defalias 'yes-or-no-p 'y-or-n-p)
(setq frame-title-format "Emacs - %f")
(add-hook 'before-save-hook 'whitespace-cleanup)

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)
(setq isearch-lax-whitespace nil)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1 scroll-conservatively 10000) ;; keyboard scroll one line at a time

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (setq-default flycheck-disabled-checkers '(javascript-jshint))
  (setq flycheck-checkers '(javascript-eslint))
  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (add-hook 'after-init-hook #'global-flycheck-mode))

(windmove-default-keybindings 'meta)

(global-unset-key "\C-t") ;; transpose-chars
(global-unset-key "\M-t") ;; transpose-words
(global-unset-key "\C-x\m") ;; mail

(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z")

(global-set-key (kbd "C-x c") 'comment-or-uncomment-region)

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

;; Web modes
(use-package js2-mode
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'ivarru-delete-spurious-whitespace-on-save)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode))
  (setq js-indent-level 2)
  (setq js-switch-indent-offset 2)
  (setq js2-allow-keywords-as-property-names t)
  (setq js2-bounce-indent-p nil)
  (setq js2-cleanup-whitespace t)
  (setq js2-global-externs
        (quote
         ("require" "define" "requirejs" "window" "describe" "it" "expect" "jasmine")))
  (setq js2-highlight-level 3)
  (setq js2-idle-timer-delay 0.5)
  (setq js2-ignored-warnings (quote ("msg.no.side.effects")))
  (setq js2-mirror-mode nil)
  (setq js2-strict-inconsistent-return-warning nil)
  (setq js2-strict-missing-semi-warning nil))
(use-package rjsx-mode :ensure t)


(use-package nodejs-repl
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-comment-style 1)
  (setq web-mode-enable-auto-closing nil)
  (setq web-mode-enable-auto-indentation nil)
  (setq web-mode-enable-auto-opening nil)
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-enable-auto-quoting nil)
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  ;; Hack to stop web-mode messing with whitespace faces
  (add-hook 'web-mode-hook 'ivarru-delete-spurious-whitespace-on-save)
  (add-hook
   'web-mode-hook
   (lambda ()
     (interactive)
     (setq-default web-mode-comment-formats (remove '("javascript" . "/*") web-mode-comment-formats))
     (add-to-list 'web-mode-comment-formats '("javascript" . "//"))
     (setq-default web-mode-comment-formats '(("javascript" . "//")))

     (set (make-local-variable 'company-backends) '(company-web-html)))
   (web-mode-set-content-type "jsx")
   (message "now set to: %s" web-mode-content-type)))

(use-package rjsx-mode :ensure t)

(use-package json-mode :ensure t)
(use-package nodejs-repl :ensure t)

;; Shell
(add-hook 'term-mode-hook (lambda() (setq show-trailing-whitespace nil)))
(add-hook 'shell-mode-hook (lambda() (setq show-trailing-whitespace nil)))

(defun clear-shell ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(add-hook
 'shell-mode-hook
 (lambda () (local-set-key (kbd "C-l") #'clear-shell)))
(add-hook
 'nodejs-repl-mode-hook
 (lambda () (local-set-key (kbd "C-l") #'clear-shell)))


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

(use-package scala-mode
  :ensure t)

(use-package company
  :ensure t
  :diminish company-mode
  :bind (("M-RET" . company-complete))
  :config
  (global-company-mode)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0)
  )

(use-package company-web :ensure t)

(use-package tern
  :ensure t
  :config  (add-hook 'js-mode-hook (lambda () (tern-mode t))))
(use-package company-tern
  :ensure
  :config (add-to-list 'company-backends 'company-tern))

(use-package magit
  :pin melpa-stable
  :ensure t
  :config
  (setq magit-cherry-buffer-name-format "*magit-cherry*")
  (setq magit-commit-arguments (quote ("--no-verify")))
  (setq magit-commit-show-diff nil)
  (setq magit-diff-buffer-name-format "*magit-diff*")
  (setq magit-diff-highlight-indentation (quote (("" . tabs))))
  (setq magit-process-buffer-name-format "*magit-process*")
  (setq magit-rebase-arguments (quote ("--autosquash")))
  (setq magit-reflog-buffer-name-format "*magit-reflog*")
  (setq magit-refs-buffer-name-format "*magit-branches*")
  (setq magit-refs-sections-hook (quote (magit-insert-local-branches)))
  (setq magit-refs-show-margin nil)
  (setq magit-revert-buffers nil)
  (setq magit-revision-buffer-name-format "*magit-commit*")
  (setq magit-stash-buffer-name-format "*magit-stash*")
  (setq magit-stashes-buffer-name-format "*magit-stashes*")
  (setq magit-status-buffer-name-format "*magit-status: %a*")
  (setq magit-visit-ref-behavior (quote (checkout-branch)))
  )

(use-package haskell-mode
  :ensure t)

;; Functions
(defun isearch-with-region(&optional start end)
  (interactive "r")
  (if (region-active-p)
      (progn
        (message "fancy search")
        (let ((string (buffer-substring-no-properties start end)))
          (deactivate-mark)
          (isearch-resume string nil nil t string nil)))
    (call-interactively 'isearch-forward-regexp)))

(defun isearch-with-region-backwards(&optional start end)
  (interactive "r")
  (if (region-active-p)
      (progn
        (message "fancy search")
        (let ((string (buffer-substring-no-properties start end)))
          (deactivate-mark)
          (isearch-resume string nil nil nil string nil)))
    (call-interactively 'isearch-backward-regexp)))


(defun ivarru-delete-spurious-whitespace ()
  (interactive)
  (let ((delete-trailing-lines t))
    (delete-trailing-whitespace))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\([^ \t\n] \\) +" nil t)
      (message "%d" (point))
      (beginning-of-line)
      (if (looking-at (concat "^ *" (regexp-quote comment-start)))
          (forward-line 1)
        (replace-match "\\1"))))
  ;; Return nil for the benefit of `write-file-functions'.
  nil)

(defun ivarru-delete-spurious-whitespace-on-save ()
  (make-variable-buffer-local 'write-file-functions)
  (add-to-list 'write-file-functions 'ivarru-delete-spurious-whitespace))

(defun save-buffer-without-whitespace-cleanup ()
  (interactive)
  (let ((b (current-buffer))) ; memorize the buffer
    (with-temp-buffer ; new temp buffer to bind the global value of before-save-hook
      (let ((write-file-functions (remove 'ivarru-delete-spurious-whitespace write-file-functions)))
        (with-current-buffer b ; go back to the current buffer, write-file-functions is now buffer-local
          (let ((write-file-functions (remove 'ivarru-delete-spurious-whitespace write-file-functions)))
            (save-buffer)))))))

;;; Grep
(defun my-compile-goto-error-same-window ()
  (interactive)
  (let ((display-buffer-overriding-action
         '((display-buffer-reuse-window
            display-buffer-same-window)
           (inhibit-same-window . nil))))
    (call-interactively #'compile-goto-error)))

(defun my-compilation-mode-hook ()
  (local-set-key (kbd "<return>") #'my-compile-goto-error-same-window))

(add-hook 'compilation-mode-hook #'my-compilation-mode-hook)

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
 '(background-mode dark)
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/saves"))))
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(company-dabbrev-ignore-case (quote keep-prefix))
 '(create-lockfiles t)
 '(css-indent-offset 2)
 '(cursor-color "#cccccc")
 '(delete-old-versions t)
 '(explicit-bash-args (quote ("--noediting" "--login" "-i")))
 '(fill-column 100)
 '(foreground-color "#cccccc")
 '(global-auto-complete-mode nil)
 '(global-font-lock-mode t)
 '(global-whitespace-mode t)
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "dist" "node_modules" "external" "coverage" "vendor" "out" "build")))
 '(grep-find-ignored-files
   (quote
    (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.lock" "package-lock.json")))
 '(helm-reuse-last-window-split-state t)
 '(helm-split-window-inside-p t)
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(kept-new-versions 2)
 '(kept-old-versions 2)
 '(minibuffer-prompt-properties
   (quote
    (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(package-selected-packages
   (quote
    (company-tern tern exec-path-from-shell which-key web-mode use-package try scala-mode rjsx-mode nodejs-repl multiple-cursors markdown-mode magit json-mode helm-swoop helm-projectile helm-ls-git haskell-mode flycheck diminish company-web)))
 '(pop-up-windows t)
 '(require-final-newline t)
 '(ruby-deep-arglist nil)
 '(same-window-regexps (quote ("*")))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(truncate-partial-width-windows nil)
 '(version-control (quote never))
 '(whitespace-style
   (quote
    (face spaces tabs newline space-mark tab-mark newline-mark trailing indentation)))
 '(winner-mode t))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 151 :width normal))))
 '(column-marker-1 ((t (:background "dark red"))))
 '(helm-buffer-process ((t (:foreground "sienna1"))))
 '(helm-ff-directory ((t (:foreground "deep sky blue"))))
 '(helm-ff-file ((t (:inherit nil :foreground "PeachPuff1"))))
 '(helm-selection ((t (:background "dark slate gray" :distant-foreground "black"))))
 '(whitespace-newline ((t (:foreground "dim gray" :weight normal))))
 '(whitespace-space ((t (:foreground "#3f4554"))))
 '(whitespace-trailing ((t (:background "#FF0000" :foreground "#FFFFFF" :inverse-video nil :underline nil :slant normal :weight bold)))))
(put 'downcase-region 'disabled nil)
