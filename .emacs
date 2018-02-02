(require 'package)

(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" .
               "http://melpa.milkbox.net/packages/"))
(add-to-list 'load-path "~/.emacs.d/lisp")

(package-initialize)


;; Emacs UI
(require 'helm-config)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(setq helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)

(global-set-key (kbd "C-s") 'isearch-with-region)
(global-set-key (kbd "C-r") 'isearch-with-region-backwards)

(helm-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq frame-title-format "Emacs - %f")
(global-auto-complete-mode 1)
(add-hook 'before-save-hook 'whitespace-cleanup)

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)
(setq isearch-lax-whitespace nil)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1
      scroll-conservatively 10000) ;; keyboard scroll one line at a time

(add-hook 'after-init-hook #'global-flycheck-mode)

(windmove-default-keybindings 'meta)

(global-unset-key "\C-t") ;; transpose-chars
(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z")

(global-set-key (kbd "M-C-<down>") 'mc/mark-next-like-this)
(global-set-key (kbd "M-C-<up>") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-r") 'rgrep)

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
   This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)



;; Web modes
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.vue$" . vue-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

(add-hook 'web-mode-hook 'ivarru-delete-spurious-whitespace-on-save)
(add-hook 'js2-mode-hook 'ivarru-delete-spurious-whitespace-on-save)
(add-hook 'css-mode-hook 'ivarru-delete-spurious-whitespace-on-save)
(add-hook 'html-mode-hook 'ivarru-delete-spurious-whitespace-on-save)
(add-hook 'vue-mode-hook 'ivarru-delete-spurious-whitespace-on-save)

;; Hack to stop web-mode messing with whitespace faces
(add-hook 'web-mode-hook
          (lambda ()
            (interactive)
            (global-whitespace-mode t)
            (setq-default web-mode-comment-formats (remove '("javascript" . "/*") web-mode-comment-formats))
            (add-to-list 'web-mode-comment-formats '("javascript" . "//"))
            (setq-default web-mode-comment-formats '(("javascript" . "//")))))

(add-hook 'web-mode-hook
          (lambda ()
            (web-mode-set-content-type "jsx")
            (message "now set to: %s" web-mode-content-type)))

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
(add-hook
 'markdown-mode-hook
 (lambda ()
   (local-set-key (kbd "M-<left>") #'windmove-left)
   (local-set-key (kbd "M-<right>") #'windmove-right)
   (local-set-key (kbd "M-<up>") #'windmove-up)
   (local-set-key (kbd "M-<down>") #'windmove-down)
   (local-set-key (kbd "<backspace>") #'backward-delete-char-untabify)))



;; Functions
(defun isearch-with-region(&optional start end)
  (interactive "r")
  (if (region-active-p)
      (progn
        (let ((string (buffer-substring-no-properties start end)))
          (deactivate-mark)
          (isearch-resume string nil nil t string nil)))
    (call-interactively 'isearch-forward-regexp)))

(defun isearch-with-region-backwards(&optional start end)
  (interactive "r")
  (if (region-active-p)
      (progn
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

(defun save-buffer-without-whitespace-cleanup  ()
  (interactive)
  (let ((b (current-buffer))) ; memorize the buffer
    (with-temp-buffer ; new temp buffer to bind the global value of before-save-hook
      (let ((write-file-functions (remove 'ivarru-delete-spurious-whitespace write-file-functions)))
        (with-current-buffer b ; go back to the current buffer, write-file-functions is now buffer-local
          (let ((write-file-functions (remove 'ivarru-delete-spurious-whitespace write-file-functions)))
            (save-buffer)))))))

(defun ivarru-define-keys (keymap &rest defs)
  (cond
   ((null keymap) (setq keymap (current-local-map)))
   ((eq keymap t) (setq keymap (current-global-map))))
  (while defs (define-key keymap (pop defs) (pop defs))))


;; Customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu nil)
 '(ac-auto-start nil)
 '(ac-js2-evaluate-calls t)
 '(ac-trigger-key "M-RET")
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(background-color "#202020")
 '(background-mode dark)
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/saves"))))
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(create-lockfiles t)
 '(css-indent-offset 2)
 '(cursor-color "#cccccc")
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes
   (quote
    ("20070e2f1b2f738568a8b1eeb53e413d427cb24a129e37951255520c51d152bf" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" default)))
 '(delete-old-versions t)
 '(explicit-bash-args (quote ("--noediting" "--login" "-i")))
 '(fill-column 120)
 '(foreground-color "#cccccc")
 '(global-auto-complete-mode t)
 '(global-font-lock-mode t)
 '(global-whitespace-mode t)
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "dist" "node_modules" "external" "coverage" "vendor" "out" "build")))
 '(grep-find-ignored-files
   (quote
    (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.lock")))
 '(helm-reuse-last-window-split-state t)
 '(helm-split-window-inside-p t)
 '(hippie-expand-dabbrev-as-symbol nil)
 '(hippie-expand-max-buffers 0)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-case-fold t)
 '(ido-create-new-buffer (quote always))
 '(ido-ignore-buffers (quote ("^*Mini" "mini" "echo")))
 '(ido-max-work-file-list 200)
 '(ido-save-directory-list-file "~/.emacs.d/cache/ido.last")
 '(ido-use-virtual-buffers t)
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(js2-allow-keywords-as-property-names t)
 '(js2-basic-offset 4)
 '(js2-bounce-indent-p nil)
 '(js2-cleanup-whitespace t)
 '(js2-global-externs
   (quote
    ("require" "define" "requirejs" "window" "describe" "it" "expect" "jasmine")))
 '(js2-highlight-level 3)
 '(js2-idle-timer-delay 0.5)
 '(js2-indent-switch-body t)
 '(js2-mirror-mode nil)
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-missing-semi-warning nil)
 '(kept-new-versions 2)
 '(kept-old-versions 2)
 '(magit-cherry-buffer-name-format "*magit-cherry*")
 '(magit-commit-arguments (quote ("--no-verify")))
 '(magit-commit-show-diff nil)
 '(magit-diff-buffer-name-format "*magit-diff*")
 '(magit-diff-highlight-indentation (quote (("" . tabs))))
 '(magit-process-buffer-name-format "*magit-process*")
 '(magit-rebase-arguments (quote ("--autosquash")))
 '(magit-reflog-buffer-name-format "*magit-reflog*")
 '(magit-refs-buffer-name-format "*magit-branches*")
 '(magit-refs-sections-hook (quote (magit-insert-local-branches)))
 '(magit-refs-show-margin nil)
 '(magit-revert-buffers nil t)
 '(magit-revision-buffer-name-format "*magit-commit*")
 '(magit-stash-buffer-name-format "*magit-stash*")
 '(magit-stashes-buffer-name-format "*magit-stashes*")
 '(magit-status-buffer-name-format "*magit-status: %a*")
 '(magit-visit-ref-behavior (quote (checkout-branch)))
 '(menu-bar-mode nil)
 '(minibuffer-prompt-properties
   (quote
    (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(package-selected-packages
   (quote
    (swift-mode scss-mode vue-mode vue-html-mode virtualenvwrapper python-mode 2048-game helm w3m w3 rjsx-mode flow-minor-mode haskell-mode yasnippet yaml-mode web-mode tern-auto-complete scala-mode nodejs-repl multiple-cursors markdown-mode json-mode js2-mode flycheck evil-magit column-marker)))
 '(pop-up-windows t)
 '(require-final-newline t)
 '(ruby-deep-arglist nil)
 '(same-window-regexps (quote ("*")))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(truncate-partial-width-windows nil)
 '(version-control (quote never))
 '(vue-modes
   (quote
    ((:type template :name nil :mode vue-html-mode)
     (:type template :name html :mode vue-html-mode)
     (:type template :name jade :mode jade-mode)
     (:type template :name pug :mode pug-mode)
     (:type template :name slm :mode slim-mode)
     (:type template :name slim :mode slim-mode)
     (:type script :name nil :mode js-mode)
     (:type script :name js :mode js2-mode)
     (:type script :name es6 :mode js2-mode)
     (:type script :name babel :mode js2-mode)
     (:type script :name coffee :mode coffee-mode)
     (:type script :name ts :mode typescript-mode)
     (:type script :name typescript :mode typescript-mode)
     (:type style :name nil :mode css-mode)
     (:type style :name css :mode css-mode)
     (:type style :name stylus :mode stylus-mode)
     (:type style :name less :mode less-css-mode)
     (:type style :name scss :mode css-mode)
     (:type style :name sass :mode ssass-mode))))
 '(web-mode-code-indent-offset 2)
 '(web-mode-comment-style 1)
 '(web-mode-enable-auto-closing nil)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-enable-auto-opening nil)
 '(web-mode-enable-auto-pairing nil)
 '(web-mode-enable-auto-quoting nil)
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
 '(whitespace-newline ((t (:foreground "dim gray" :weight normal))))
 '(whitespace-space ((t (:foreground "#3f4554"))))
 '(whitespace-trailing ((t (:background "#FF0000" :foreground "#FFFFFF" :inverse-video nil :underline nil :slant normal :weight bold)))))
