(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
    '("melpa" .
      "http://melpa.milkbox.net/packages/"))

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/noctilux-theme-20150723.747/")

(package-initialize)
(require 'multiple-cursors)

(require 'ido)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-auto-merge-work-directories-length -1
      ido-case-fold  t                 ; be case-insensitive
      ido-confirm-unique-completion t
      ido-create-new-buffer 'always
      ido-enable-flex-matching t
      ido-enable-last-directory-history t ; remember last used dirs
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 20              ; don't spam my minibuffer
      ido-max-work-directory-list 10   ; should be enough
      ido-max-work-file-list      100   ; remember many
      ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
      ido-use-filename-at-point nil    ; don't use filename at point (annoying)
      ido-use-url-at-point nil         ; don't use url at point (annoying)
      ido-use-virtual-buffers t
      ido-ignore-buffers ;; ignore these guys
      '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
        "^\*compilation" "^\*GTAGS" "^session\.*")
      ido-work-directory-list '("~/"
                                "~/cxense/cx/services/analytics/src/main/resources/com/cxense/analytics/webapp"
                                "~/cxense/cx/modules/cx-core/src/main/resources/com/cxense/webapp"))

(menu-bar-mode -99)

;; when using ido, the confirmation is rather annoying...
 (setq confirm-nonexistent-file-or-buffer nil)


(defalias 'yes-or-no-p 'y-or-n-p)
(transient-mark-mode 1) ;; highlight selection
(global-font-lock-mode 1)
(show-paren-mode 1)

(require 'linum)
;;(global-linum-mode)
(define-global-minor-mode my-gobal-linum-mode global-linum-mode
  (lambda ()
    (when (not (memq major-mode
                     (list 'slime-repl-mode 'shell-mode 'term-mode)))
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
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode))

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

(add-hook 'web-mode-hook 'ivarru-delete-spurious-whitespace-on-save)
(add-hook 'js2-mode-hook 'ivarru-delete-spurious-whitespace-on-save)
(add-hook 'css-mode-hook 'ivarru-delete-spurious-whitespace-on-save)
(add-hook 'html-mode-hook 'ivarru-delete-spurious-whitespace-on-save)
(add-hook 'web-mode-hook (lambda () (interactive) (column-marker-1 121)))
(add-hook 'js2-mode-hook (lambda () (interactive) (column-marker-1 121)))
(add-hook 'css-mode-hook (lambda () (interactive) (column-marker-1 121)))
(add-hook 'html-mode-hook (lambda () (interactive) (column-marker-1 121)))


;; (defun ivarru-delete-spurious-whitespace-on-save ()
;;   (make-variable-buffer-local 'write-file-functions)
;;   ;; Notice that delete-trailing-whitespace returns nil.
;;   (add-to-list 'write-file-functions 'delete-trailing-whitespace)
;;   (add-to-list 'write-file-functions
;;                (lambda ()
;;                  (save-excursion
;;                    (replace-regexp "\\(^ *\\(?:[^ /]\\|/[^/]\\).*[^ \n]\\)  +"
;;                                    "\\1 " nil (point-min) (point-max))))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(flycheck-eslintrc "~/.eslintrc")
 '(flycheck-temp-prefix "/tmp/flycheck")
 '(foreground-color "#cccccc")
 '(global-whitespace-mode t)
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "dist" "node_modules" "external")))
 '(grep-find-ignored-files
   (quote
    (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo")))
 '(hippie-expand-dabbrev-as-symbol nil)
 '(hippie-expand-max-buffers 0)
 '(ido-case-fold t)
 '(imenu-auto-rescan t)
 '(inhibit-startup-screen t)
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
 '(menu-bar-mode t)
 '(minibuffer-prompt-properties
   (quote
    (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(require-final-newline t)
 '(ruby-deep-arglist nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil)
 '(truncate-partial-width-windows nil)
 '(whitespace-style
   (quote
    (face spaces tabs newline space-mark tab-mark newline-mark trailing indentation))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-marker-1 ((t (:background "dark red"))))
 '(whitespace-newline ((t (:foreground "disabledControlTextColor" :weight normal))))
 '(whitespace-space ((t (:foreground "disabledControlTextColor"))))
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
(global-set-key (kbd "M-RET") 'hippie-expand)
(global-set-key (kbd "M-s") 'er/expand-region)
(global-set-key (kbd "M-C-<down>") 'mc/mark-next-like-this)
(global-set-key (kbd "M-C-<up>") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-x ?") 'locate)

;; (global-set-key (kbd "C-M-RET") 'find-file-at-point)


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
                             "python -c 'import sys,json; data=json.loads(sys.stdin.read()); print json.dumps(data,sort_keys=True,indent=4).decode(\"unicode_escape\").encode(\"utf8\",\"replace\")'" (current-buffer) t)))


(require 'js2-mode)
(define-key js2-mode-map (kbd "C-c C-f") 'beautify-json)
(put 'downcase-region 'disabled nil)
(setq inferior-js-program-command "/usr/bin/node")

(setq isearch-lax-whitespace nil)
(put 'upcase-region 'disabled nil)

(require 'org)
(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))


(setq-default fill-column 100)
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
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
(setq flycheck-checkers '(javascript-eslint))
(flycheck-add-mode 'javascript-eslint 'web-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)


(winner-mode 1)

(fset 'initlayout
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 51 24 51 24 43 M-right M-right 24 50 M-down 134217848 115 104 101 108 108 return 134217848 114 101 110 97 44 101 backspace backspace 109 101 45 98 117 102 102 101 114 return 42 115 104 101 108 108 45 103 114 117 110 116 42 return 134217848 115 104 101 108 108 return M-left M-left 134217848 109 97 103 105 116 45 115 116 97 116 117 115 return 99 120 tab 99 120 return M-up 24 48] 0 "%d")) arg)))

(add-hook 'term-mode-hook (lambda() (setq show-trailing-whitespace nil)))
