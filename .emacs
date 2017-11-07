(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
    '("melpa" .
      "http://melpa.milkbox.net/packages/"))
(add-to-list 'load-path "~/.emacs.d/lisp")

(package-initialize)
(require 'multiple-cursors)

;; (require 'ido)
;; (ido-mode t)
(require 'helm-config)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(setq helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)

(helm-mode 1)

(scroll-bar-mode -1)
(menu-bar-mode -99)


(defalias 'yes-or-no-p 'y-or-n-p)
(transient-mark-mode 1) ;; highlight selection
(global-font-lock-mode 1)
(show-paren-mode 1)

(setq frame-title-format "Emacs - %f")
(setq echo-keystrokes .1)

(require 'linum)
(define-global-minor-mode my-gobal-linum-mode global-linum-mode
  (lambda ()
    (when
        (and
         (not (string-match "magit-" (symbol-name major-mode)))
         (not (memq major-mode
               (list 'slime-repl-mode 'shell-mode 'term-mode))))
      (linum-mode))))
(my-gobal-linum-mode 1)

(setq
  backup-by-copying t      ; don't clobber symlinks
  backup-directory-alist
   '(("." . "~/.emacs.d/saves"))    ; don't litter my fs tree
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t
  create-lockfiles t) ; disable dot-hash lock files

;; tabs
(setq-default tab-width 4)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)


;; javascript
(autoload 'web-mode "web-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

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
  (let ((b (current-buffer)))   ; memorize the buffer
    (with-temp-buffer ; new temp buffer to bind the global value of before-save-hook
      (let ((write-file-functions (remove 'ivarru-delete-spurious-whitespace write-file-functions)))
        (with-current-buffer b  ; go back to the current buffer, write-file-functions is now buffer-local
          (let ((write-file-functions (remove 'ivarru-delete-spurious-whitespace write-file-functions)))
            (save-buffer)))))))

(add-hook 'web-mode-hook 'ivarru-delete-spurious-whitespace-on-save)
(add-hook 'js2-mode-hook 'ivarru-delete-spurious-whitespace-on-save)
(add-hook 'css-mode-hook 'ivarru-delete-spurious-whitespace-on-save)
(add-hook 'html-mode-hook 'ivarru-delete-spurious-whitespace-on-save)
(add-hook 'web-mode-hook (lambda () (interactive) (column-marker-1 121)))

(add-hook 'js2-mode-hook (lambda () (interactive) (column-marker-1 121)))
(add-hook 'css-mode-hook (lambda () (interactive) (column-marker-1 121)))
(add-hook 'html-mode-hook (lambda () (interactive) (column-marker-1 121)))

;; Hack to stop web-mode messing with whitespace faces
(add-hook 'web-mode-hook
          (lambda ()
            (interactive)
            (global-whitespace-mode t)
            (setq-default web-mode-comment-formats (remove '("javascript" . "/*") web-mode-comment-formats))
            (add-to-list 'web-mode-comment-formats '("javascript" . "//"))
            (setq-default web-mode-comment-formats '(("javascript" . "//")))))






(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-js2-evaluate-calls t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(background-color "#202020")
 '(background-mode dark)
 '(cursor-color "#cccccc")
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes
   (quote
    ("20070e2f1b2f738568a8b1eeb53e413d427cb24a129e37951255520c51d152bf" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" default)))
 '(explicit-bash-args (quote ("--noediting" "--login" "-i")))
 '(foreground-color "#cccccc")
 '(global-auto-complete-mode t)
 '(global-whitespace-mode t)
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "dist" "node_modules" "external" "coverage" "vendor" "out")))
 '(grep-find-ignored-files
   (quote
    (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.lock")))
 '(helm-reuse-last-window-split-state t)
 '(helm-split-window-in-side-p t)
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
 '(js2-indent-switch-body t)
 '(js2-mirror-mode nil)
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-missing-semi-warning t)
 '(magit-cherry-buffer-name-format "*magit-cherry*")
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
 '(menu-bar-mode t)
 '(minibuffer-prompt-properties
   (quote
    (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(package-selected-packages
   (quote
    (python-mode 2048-game helm w3m w3 rjsx-mode flow-minor-mode haskell-mode yasnippet yaml-mode web-mode tern-auto-complete scala-mode nodejs-repl multiple-cursors markdown-mode json-mode js2-mode flycheck evil-magit column-marker)))
 '(pop-up-windows t)
 '(require-final-newline t)
 '(ruby-deep-arglist nil)
 '(same-window-regexps (quote ("*")))
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil)
 '(truncate-partial-width-windows nil)
 '(web-mode-code-indent-offset 2)
 '(web-mode-comment-style 1)
 '(web-mode-enable-auto-closing nil)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-enable-auto-opening nil)
 '(web-mode-enable-auto-pairing nil)
 '(web-mode-enable-auto-quoting nil)
 '(whitespace-style
   (quote
    (face spaces tabs newline space-mark tab-mark newline-mark trailing indentation))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-marker-1 ((t (:background "dark red"))))
 '(whitespace-newline ((t (:foreground "dim gray" :weight normal))))
 '(whitespace-space ((t (:foreground "#3f4554"))))
 '(whitespace-trailing ((t (:background "#FF0000" :foreground "#FFFFFF" :inverse-video nil :underline nil :slant normal :weight bold)))))


(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

;;(add-to-list 'load-path "~/.emacs.d/vendor/")
;;(require 'handlebars-mode)
(setq handlebars-basic-offset 4)


;; scroll one line at a time (less "jumpy" than defaults)

(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1
      scroll-conservatively 10000) ;; keyboard scroll one line at a time


(windmove-default-keybindings 'meta)


(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
   This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

(defun clear-shell ()
   (interactive)
   (let ((comint-buffer-maximum-size 0))
     (comint-truncate-buffer)))

(add-hook 'shell-mode-hook
          (lambda () (local-set-key (kbd "C-l") #'clear-shell)))

(add-hook 'nodejs-repl-mode-hook
  (lambda () (local-set-key (kbd "C-l") #'clear-shell)))


;; SCSS-mode
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/folder-where-you-put-scss-mode-el"))
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)
(setq sgml-basic-offset 4)


;;(add-to-list 'load-path "~/.emacs.d/expand-region.el")

;;(require 'expand-region)

(add-hook 'before-save-hook 'whitespace-cleanup)


;; mustasche test
;;(add-to-list 'load-path "~/.emacs.d/mustache-mode.el")
;;(require 'mustache-mode)



;; Key bindings
(global-unset-key "\C-t") ;; transpose-chars
(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z")
(global-set-key (kbd "C-M-y") '(lambda ()
   (interactive)
   (popup-menu 'yank-menu)))
(global-set-key (kbd "C-x c") 'comment-or-uncomment-region)
;;(global-set-key (kbd "M-RET") 'hippie-expand)
(global-set-key (kbd "M-RET") 'auto-complete)
(global-set-key (kbd "M-s") 'er/expand-region)
(global-set-key (kbd "M-C-<down>") 'mc/mark-next-like-this)
(global-set-key (kbd "M-C-<up>") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-x ?") 'locate)
(global-set-key (kbd "M-r") 'rgrep)



(add-hook 'magit-mode-hook
          (lambda ()
                 (local-set-key (kbd "C-<down>") 'magit-goto-next-section)
                 (local-set-key (kbd "C-<up>") 'magit-goto-previous-section)))
(setq magit-last-seen-setup-instructions "1.4.0")


(defun ivarru-define-keys (keymap &rest defs)
  (cond
   ((null keymap) (setq keymap (current-local-map)))
   ((eq keymap t) (setq keymap (current-global-map))))
  (while defs (define-key keymap (pop defs) (pop defs))))


(setq ns-pop-up-frames nil)




(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -c 'import sys,json; data=json.loads(sys.stdin.read()); print json.dumps(data,sort_keys=True,indent=4,separators=(\",\", \": \")).decode(\"unicode_escape\").encode(\"utf8\",\"replace\")'" (current-buffer) t)))

;;(define-key json-mode-map (kbd "C-c C-f") 'beautify-json)


(require 'js2-mode)
(define-key js2-mode-map (kbd "C-c C-f") 'beautify-json)
(put 'downcase-region 'disabled nil)
(setq inferior-js-program-command "/usr/bin/node")

(setq isearch-lax-whitespace nil)
(put 'upcase-region 'disabled nil)

(require 'org)
(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))


(setq-default fill-column 80)
(setq column-number-mode t)
(put 'erase-buffer 'disabled nil)

;; grep mode open in same window

(eval-after-load "compile"
'(defun compilation-goto-locus (msg mk end-mk)
  "Jump to an error corresponding to MSG at MK.
All arguments are markers.  If END-MK is non-nil, mark is set there
and overlay is highlighted between MK and END-MK."
  ;; Show compilation buffer in other window, scrolled to this error.
  (let* ((from-compilation-buffer (eq (window-buffer (selected-window))
                  (marker-buffer msg)))
     ;; Use an existing window if it is in a visible frame.
     (pre-existing (get-buffer-window (marker-buffer msg) 0))
     (w (if (and from-compilation-buffer pre-existing)
        ;; Calling display-buffer here may end up (partly) hiding
        ;; the error location if the two buffers are in two
        ;; different frames.  So don't do it if it's not necessary.
        pre-existing
      (let ((display-buffer-reuse-frames t)
        (pop-up-windows t))
        ;; Pop up a window.
        (display-buffer (marker-buffer msg)))))
     (highlight-regexp (with-current-buffer (marker-buffer msg)
             ;; also do this while we change buffer
             (compilation-set-window w msg)
             compilation-highlight-regexp)))
;; Ideally, the window-size should be passed to `display-buffer' (via
;; something like special-display-buffer) so it's only used when
;; creating a new window.
(unless pre-existing (compilation-set-window-height w))

(switch-to-buffer (marker-buffer mk))

;; If narrowing gets in the way of going to the right place, widen.
(unless (eq (goto-char mk) (point))
  (widen)
  (goto-char mk))
(if end-mk
    (push-mark end-mk t)
  (if mark-active (setq mark-active)))
;; If hideshow got in the way of
;; seeing the right place, open permanently.
(dolist (ov (overlays-at (point)))
  (when (eq 'hs (overlay-get ov 'invisible))
    (delete-overlay ov)
    (goto-char mk)))

(when highlight-regexp
  (if (timerp next-error-highlight-timer)
      (cancel-timer next-error-highlight-timer))
  (unless compilation-highlight-overlay
    (setq compilation-highlight-overlay
      (make-overlay (point-min) (point-min)))
    (overlay-put compilation-highlight-overlay 'face 'next-error))
  (with-current-buffer (marker-buffer mk)
    (save-excursion
      (if end-mk (goto-char end-mk) (end-of-line))
      (let ((end (point)))
    (if mk (goto-char mk) (beginning-of-line))
    (if (and (stringp highlight-regexp)
         (re-search-forward highlight-regexp end t))
        (progn
          (goto-char (match-beginning 0))
          (move-overlay compilation-highlight-overlay
                (match-beginning 0) (match-end 0)
                (current-buffer)))
      (move-overlay compilation-highlight-overlay
            (point) end (current-buffer)))
    (if (or (eq next-error-highlight t)
        (numberp next-error-highlight))
        ;; We want highlighting: delete overlay on next input.
        (add-hook 'pre-command-hook
              'compilation-goto-locus-delete-o)
      ;; We don't want highlighting: delete overlay now.
      (delete-overlay compilation-highlight-overlay))
    ;; We want highlighting for a limited time:
    ;; set up a timer to delete it.
    (when (numberp next-error-highlight)
      (setq next-error-highlight-timer
        (run-at-time next-error-highlight nil
                 'compilation-goto-locus-delete-o)))))))
(when (and (eq next-error-highlight 'fringe-arrow))
  ;; We want a fringe arrow (instead of highlighting).
  (setq next-error-overlay-arrow-position
        (copy-marker (line-beginning-position)))))))

(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)

(setq-default flycheck-disabled-checkers '(javascript-jshint))
(setq flycheck-checkers '(javascript-eslint))
(setq flycheck-eslintrc "~/.eslintrc.json")

(flycheck-add-mode 'javascript-eslint 'web-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)


(winner-mode 1)



(add-hook 'term-mode-hook (lambda() (setq show-trailing-whitespace nil)))
(add-hook 'shell-mode-hook (lambda() (setq show-trailing-whitespace nil)))


(global-auto-complete-mode 1)


(fset 'initlayout
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 51 24 51 24 43 M-right M-right 24 50 134217848 115 104 101 108 108 return 134217848 110 backspace 109 97 103 105 116 45 115 116 97 116 117 115 return 114 101 112 111 tab 101 118 101 tab 101 backspace 115 tab 106 tab return 24] 0 "%d")) arg)))

(fset 'go-to-magit-status
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 98 42 109 97 103 105 116 58 return] 0 "%d")) arg)))
(global-set-key (kbd "C-x m") 'go-to-magit-status)

(defvar my-ediff-last-windows nil)

(defun my-store-pre-ediff-winconfig ()
  (setq my-ediff-last-windows (current-window-configuration)))

(defun my-restore-pre-ediff-winconfig ()
  (set-window-configuration my-ediff-last-windows))

(add-hook 'ediff-before-setup-hook #'my-store-pre-ediff-winconfig)
(add-hook 'ediff-quit-hook #'my-restore-pre-ediff-winconfig)


(add-hook
 'json-mode-hook
 (lambda ()
   (make-local-variable 'js-indent-level)
   (setq js-indent-level 2)))

(setq calendar-week-start-day 1)


(add-hook
 'markdown-mode-hook
 (lambda ()
   (local-set-key (kbd "M-<left>") #'windmove-left)
   (local-set-key (kbd "M-<right>") #'windmove-right)
   (local-set-key (kbd "M-<up>") #'windmove-up)
   (local-set-key (kbd "M-<down>") #'windmove-down)
   (local-set-key (kbd "<backspace>") #'backward-delete-char-untabify)))
