;; -*- lexical-binding: t; -*-

;;; Emacs config — rebooted.
;; The pre-reboot config is archived verbatim in emacs-graveyard.el.
;; Anything old that gets revived should be modernized and cleaned up on
;; its way back in, one increment at a time.

;;; Font
(set-face-attribute 'default nil :family "Ubuntu Mono" :height 180)

;;; Sensible defaults
(load-file "~/repos/dotfiles/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/bind-commenting-and-uncommenting)

;;; Native compilation
;; Async native-comp of installed packages (agent-shell, shell-maker, ...)
;; emits spurious "function not known to be defined" warnings for
;; autoloaded / macro-generated functions. They're harmless package lint
;; noise; don't spam the startup buffer with them.
(add-to-list 'warning-suppress-types '(native-compiler))

;;; straight.el bootstrap
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
(setq straight-use-package-by-default t)

;;; macOS modifiers
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      select-enable-clipboard t)

;;; Window / buffer navigation
;; M-<left>/<right>/<up>/<down> to move focus between window splits
(windmove-default-keybindings 'meta)

;;; UI
(global-hl-line-mode)
(blink-cursor-mode 0)
(menu-bar-mode 1)
(tool-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(winner-mode t)
(setq frame-title-format "Emacs")

;; Treesit indicator: shows [ts] bright when a treesit parser is active
(add-to-list 'mode-line-misc-info
             '(" "
               (:eval (if (treesit-parser-list)
                          (propertize "[ts]" 'face 'success)
                        (propertize "[ts]" 'face 'shadow)))))

(defalias 'list-buffers 'ibuffer)
(setq display-buffer-alist
      '((".*"
         (display-buffer-same-window)
         (inhibit-same-window . nil))))

;; major modes + eglot (TypeScript, Python)
;; The ts-modes (:mode mappings below) are needed on Emacs 30 AND 31 --
;; tree-sitter major modes are still *not* selected by default even in
;; 31.1 (the new `treesit-enabled-modes' option defaults to nil).
;; Grammars are auto-installed via `treesit-auto-install-grammar' (new
;; in Emacs 31, default `ask') using the recipes in
;; `treesit-language-source-alist' below; on Emacs 30 they had to be
;; installed manually with M-x treesit-install-language-grammar.
;; Old typescript-mode/tsx derived modes/prettier-js are obsolete --
;; formatting is handled by eglot (server) when available.
(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")
        (python "https://github.com/tree-sitter/tree-sitter-python")))
;; (Grammars for all of these are already compiled; to refresh one:
;;  M-x treesit-install-language-grammar RET <lang>)

(defun my-eglot-sanitize-markup (orig-fn markup &optional mode)
  "Convert raw HTML entities (like &nbsp;) in LSP hover markup before rendering."
  (if (and (stringp markup) (string-match-p "&[a-zA-Z0-9#]+;" markup))
      (let ((clean (with-temp-buffer
                     (insert markup)
                     (goto-char (point-min))
                     (while (search-forward "&nbsp;" nil t)
                       (replace-match " "))
                     (goto-char (point-min))
                     (while (search-forward "&gt;" nil t)
                       (replace-match ">"))
                     (goto-char (point-min))
                     (while (search-forward "&lt;" nil t)
                       (replace-match "<"))
                     (goto-char (point-min))
                     (while (search-forward "&amp;" nil t)
                       (replace-match "&"))
                     (goto-char (point-min))
                     (while (search-forward "&quot;" nil t)
                       (replace-match "\""))
                     (buffer-string))))
        (funcall orig-fn clean mode))
    (funcall orig-fn markup mode)))

(defun my-eglot-hover-multiline (orig-fn cb &rest args)
  "Allow Eglot hover docstrings to display multi-line in the echo area.
Overrides Eglot's default 1-line truncation passed to ElDoc via `:echo pos'."
  (apply orig-fn
         (lambda (info &rest plist)
           (apply cb info (plist-put plist :echo info)))
         args))

(defun my-eglot-rich-capf-annotations (orig-fn &rest args)
  "Enrich Eglot completion candidates in Vertico with prefix kind tags
and resolved LSP type signatures/parameters/return types."
  (let ((res (apply orig-fn args)))
    (if (and (consp res) (>= (length res) 3))
        (let* ((beg (nth 0 res))
               (end (nth 1 res))
               (table (nth 2 res))
               (plist (copy-sequence (nthcdr 3 res)))
               (docsig-fn (plist-get plist :company-docsig))
               (doc-buffer-fn (plist-get plist :company-doc-buffer)))
          (setq plist
                (plist-put
                 plist :affixation-function
                 (lambda (candidates)
                   (mapcar
                    (lambda (cand)
                      (let* ((item (get-text-property 0 'eglot--lsp-item cand))
                             (kind-id (and item (plist-get item :kind)))
                             (kind (and kind-id (alist-get kind-id eglot--kind-names)))
                             ;; 1. Try detail string (TypeScript, etc.)
                             (raw-detail (or (and docsig-fn (funcall docsig-fn cand))
                                             (and item (plist-get item :detail))))
                             (clean-detail
                              (when (stringp raw-detail)
                                (let* ((s (string-trim raw-detail))
                                       ;; Remove leading LSP prefix classifier e.g. (method)
                                       (s (replace-regexp-in-string "^([a-zA-Z ]+)[ \t]*" "" s))
                                       ;; Collapse embedded newlines/indentation to a single clean line
                                       (s (replace-regexp-in-string "[\r\n\t ]+" " " s)))
                                  s)))
                             ;; 2. For servers like Pyright that put signatures in documentation markdown
                             (doc-sig
                              (when (or (null clean-detail) (string-empty-p clean-detail))
                                (let* ((raw-doc (or (and item (plist-get item :documentation))
                                                    (when doc-buffer-fn
                                                      (when-let* ((buf (funcall doc-buffer-fn cand)))
                                                        (with-current-buffer buf (buffer-string))))))
                                       (doc-str (cond
                                                 ((stringp raw-doc) raw-doc)
                                                 ((and (listp raw-doc) (plist-get raw-doc :value))
                                                  (plist-get raw-doc :value))
                                                 (t nil))))
                                  (when doc-str
                                    (with-temp-buffer
                                      (insert doc-str)
                                      (goto-char (point-min))
                                      (let (found)
                                        (while (and (not found) (not (eobp)))
                                          (let ((line (string-trim (buffer-substring (line-beginning-position) (line-end-position)))))
                                            (unless (or (string-empty-p line)
                                                        (string-prefix-p "```" line)
                                                        (string-prefix-p "---" line))
                                              (setq found (replace-regexp-in-string "^([a-zA-Z ]+)[ \t]*" "" line)))
                                            (forward-line 1)))
                                        (when found
                                          (replace-regexp-in-string "[\r\n\t ]+" " " found))))))))
                             (sig (or (and clean-detail (not (string-empty-p clean-detail)) clean-detail)
                                      doc-sig))
                             (prefix (if kind
                                         (propertize (format "%-12s " (format "[%s]" kind))
                                                     'face 'font-lock-type-face)
                                       ""))
                             (suffix (if (and (stringp sig) (not (string-empty-p sig)))
                                         (propertize (format "  %s" sig)
                                                     'face 'font-lock-doc-face)
                                       "")))
                        (list cand prefix suffix)))
                    candidates))))
          (append (list beg end table) plist))
      res)))

(use-package eglot
  ;; built into Emacs 30; servers used by default here:
  ;;   typescript-ts-mode/tsx-ts-mode -> typescript-language-server
  ;;   python-ts-mode                 -> pyright
  :hook ((python-ts-mode typescript-ts-mode tsx-ts-mode) . eglot-ensure)
  :config
  (setq eglot-confirm-server-initiated-edits nil)
  (advice-add #'eglot--format-markup :around #'my-eglot-sanitize-markup)
  (advice-add #'eglot-hover-eldoc-function :around #'my-eglot-hover-multiline)
  (advice-add #'eglot-completion-at-point :around #'my-eglot-rich-capf-annotations))

(use-package python
  :straight nil
  :mode (("\\.py\\'" . python-ts-mode))
  :custom
  (python-indent-offset 2))

(use-package typescript
  :straight nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (setq-default typescript-indent-level 2
                js-indent-level 2))

;;; Markdown (used by eglot to render LSP docstrings/hover info)
(use-package markdown-mode
  :custom
  ;; Hide backticks and markup characters in rendered markdown
  (markdown-hide-markup t))

;;; Eldoc: dynamic rich documentation in the echo area
;; Allow the bottom echo area / minibuffer to dynamically expand to show
;; rendered multi-line docstrings and signatures as you navigate.
(setq eldoc-documentation-strategy #'eldoc-documentation-default
      eldoc-echo-area-use-multiline-p t
      resize-mini-windows t
      max-mini-window-height 0.35
      eldoc-echo-area-display-truncation-message nil
      eldoc-idle-delay 0.1)

;;; Help buffers: fixed window at the bottom, not same-window
;; The blanket display-buffer-alist rule below sends *Help*, *eglot-help*
;; etc. into the current window, stomping on the code you were reading.
;; These more specific rules pin them to a dedicated resizable bottom
;; window instead. M-<arrow> / C-x o to jump in, `q' to close.
(add-to-list 'display-buffer-alist
             '(("\\`\\*\\(Help\|eglot-help\|eldoc\\|flymake\\).*\\*"
                (display-buffer-reuse-window display-buffer-below-selected)
                (window-height . 0.4))))

;; Full docs for the symbol at point: eglot feeds its complete server
;; docs (signature + docstring) through eldoc, and `eldoc-doc-buffer'
;; renders them in *eldoc* -- displayed in the pinned bottom window per
;; the display-buffer-alist rule above. This is also the stock C-h .
;; binding in Emacs 28+, kept here for explicitness.
(define-key global-map (kbd "C-h .") #'eldoc-doc-buffer)

;;; Minibuffer-based code completion: Vertico + Consult
;; Route in-buffer completion (eglot LSP completions, etc.) through the
;; expanding minibuffer at the bottom instead of a floating inline popup.
;; - M-RET triggers completion explicitly.
;; - Marginalia annotates candidates with LSP types, signatures, and doc strings.
;; - Orderless enables multi-term fuzzy drilldown filtering in the minibuffer.
(setq completion-in-region-function #'consult-completion-in-region)
(global-set-key (kbd "M-RET") #'completion-at-point)

;;; agent-shell -- LLM agent shells in Emacs via ACP (Agent Client Protocol)
;; We use the Pi agent. Pi manages its own credentials natively via /login
;; (stored in ~/.pi/agent/auth.json), so no API-key handling is needed here.
;;
;; Start with:  M-x agent-shell-pi-start-agent
(use-package agent-shell
  :config
  (setq agent-shell-pi-environment
        (agent-shell-make-environment-variables :inherit-env t)))

;;; gptel -- LLM client via OpenRouter, with Emacs introspection tools
(load-file "~/repos/dotfiles/gptel.el")

;;; which-key (built into Emacs 30+, no package needed)
(which-key-mode)

;;; Minibuffer completion: vertico + orderless + marginalia
(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t))

(use-package orderless
  :custom
  ;; orderless first; basic keeps standard prefix matching fallback working
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

;; Persist minibuffer history across sessions (complements vertico)
(savehist-mode)

;; Persisted history of opened files: recentf tracks them and writes
;; its list to recentf-save-file (periodically and on exit).
;; consult-buffer (C-x b) includes the recents; C-x C-r opens the
;; recents list directly.
(recentf-mode)
(setq recentf-max-saved-items 500
      recentf-exclude
      '("/\.git/" "node_modules" "\.elc$" "\.eln$"
        "/\.cache/" "\.DS_Store"))

;;; Consult -- search/navigation commands riding on completing-read
;; consult-line: "swoop" (old C-M-s helm-swoop binding kept)
;; consult-buffer: buffers+files+recentf   consult-ripgrep: project-wide grep
(use-package consult
  :bind (
         ("C-M-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-r" . consult-ripgrep)
         ;; C-x p f -> fuzzy project file picker (override built-in
         ;; project-find-file with consult-find, still under project.el)
         (:map project-prefix-map
               ("f" . consult-find))))

;;; Magit
(use-package magit)

;;; Node / nvm
;; Emacs is usually launched from the dock, so it never runs ~/.zshrc and
;; therefore doesn't have nvm's node on PATH. As a result subprocesses like
;; compile resolve the wrong node/turbo (e.g. /opt/homebrew/bin) instead of
;; the nvm `default` version your interactive shell uses. Read the alias and
;; prepend that version's bin to PATH + exec-path so compile matches your shell.
(let* ((nvm-dir (expand-file-name "~/.nvm"))
       (alias-file (expand-file-name "default" (expand-file-name "alias" nvm-dir)))
       (version (and (file-exists-p alias-file)
                     (string-trim (with-temp-buffer
                                    (insert-file-contents alias-file)
                                    (buffer-string)))))
       (node-bin (when version
                   (expand-file-name
                    (format "versions/node/%s/bin" version) nvm-dir))))
  (when (and node-bin (file-directory-p node-bin))
    (add-to-list 'exec-path node-bin)
    (setenv "PATH" (mapconcat #'identity
                               (cons node-bin
                                     (split-string (getenv "PATH") path-separator t))
                               path-separator))))

;;; Compilation
;; turbo (Turborepo) 2.x launches an interactive full-screen TUI when it
;; detects a terminal, emitting alternate-screen/cursor/mouse escape
;; sequences that garbage-up *Compilation* buffers. Emacs' `compile'
;; hands the process a pty, so turbo thinks it's interactive. The robust
;; version-independent fix is to use a pipe instead of a pty: turbo then
;; sees no TTY and falls back to plain streaming output (still ANSI-colored).
;; (TURBO_UI=false won't reliably work -- turbo 2.8.1 may ignore it.)
(setq compilation-process-connection-type nil)

;; fallback for turbo versions that still try the TUI: force stream mode
(add-to-list 'compilation-environment "TURBO_UI=false")
