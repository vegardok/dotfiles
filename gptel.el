;;; gptel.el --- gptel: LLM client via OpenRouter -*- lexical-binding: t; -*-

;;; gptel -- LLM integration through OpenRouter (OpenAI-compatible endpoint).
;;
;; Auth design: no API key lives in this repo. The OpenRouter key used by
;; the Pi agent is stored natively in ~/.pi/agent/auth.json and read from
;; there (fallback: $OPENROUTER_API_KEY).
;; Install + require the package (straight handles installation).

(straight-use-package 'gptel)
(require 'gptel)

;;; API key -- reuse Pi's stored OpenRouter credential

(defun my-gptel-openrouter-key ()
  "Return the OpenRouter API key from Pi's auth store.
Falls back to the OPENROUTER_API_KEY environment variable."
  (or (ignore-errors
        (let* ((file (expand-file-name "~/.pi/agent/auth.json"))
               (data (json-read-file file))
               (entry (cdr (assq 'openrouter data))))
          (cdr (assq 'key entry))))
      (getenv "OPENROUTER_API_KEY")))

;;; Register the OpenRouter backend

(gptel-make-openai "OpenRouter"
  :host "openrouter.ai"
  :endpoint "/api/v1/chat/completions"
  :stream t
  :key #'my-gptel-openrouter-key
  ;; Add/remove models freely; catalog: https://openrouter.ai/models
  ;; Slugs verified against the live /api/v1/models catalog.
  :models '(anthropic/claude-opus-5
            anthropic/claude-sonnet-5
            google/gemini-3.1-pro-preview
            google/gemini-3.7-flash
            openai/gpt-5.6-luna
            openai/gpt-5-codex
            z-ai/glm-5.3-flash
            deepseek/deepseek-v4-flash-0731))

;;; Defaults

(setq-default
 gptel-model 'anthropic/claude-sonnet-5
 gptel-backend (gptel-get-backend "OpenRouter"))

;;; System prompts (directives)

(setq gptel-directives
      (append gptel-directives
              '(;; Emacs tutor -- pairs with the introspection tools below
                (emacs-tutor
                 . "You are an experienced Emacser helping me learn GNU Emacs deeply.
When asked about functions, variables or faces, verify with the provided
introspection tools instead of guessing from memory. Prefer idiomatic,
modern solutions that work with vanilla Emacs and the packages mentioned
in the conversation. Explain concepts concisely, then show by example.")
                (pair-programmer
                 . "You are my pair programming partner inside Emacs.
Be direct and concise. Propose minimal, focused changes rather than
wholesale rewrites; explain trade-offs briefly. When unsure about existing
code or APIs, say what you know and ask for the missing context. Assume
Emacs Lisp for editor work; match the indentation and style of the code
already shown."))))

;;; Tools -- let the model interact with Emacs itself
;;
;; gptel ships no tools by default; these three cover the core
;; introspection loop (what exists -> how documented -> where defined).
;; Model tools are toggled from gptel's transient menu (C-u C-c RET),
;; where they appear under category "emacs".

(defun my-gptel--symbol-documentation (sym)
  "Return the *Help* documentation text for SYM."
  (save-window-excursion
    (describe-symbol sym)
    (with-current-buffer "*Help*"
      (buffer-string))))

(gptel-make-tool
 :name "describe_symbol"
 :function
 (lambda (symbol-name)
   (condition-case err
       (my-gptel--symbol-documentation (intern symbol-name))
     (error (format "No such symbol %S (%s)" symbol-name err))))
 :description
 "Show the full Emacs documentation (docstring, arguments, references) for
an interactive command, function, variable or face."
 :args
 (list '(:name "symbol_name"
               :type string
               :description "The name of the Emacs Lisp symbol to document"))
 :category "emacs")

(gptel-make-tool
 :name "read_symbol_source"
 :function
 (lambda (symbol-name)
   (condition-case err
       (let* ((sym (intern symbol-name))
              (loc (cond
                     ((fboundp sym) (find-function-noselect sym))
                     ((boundp sym)  (find-variable-noselect sym))
                     (t (error "Symbol %S is not defined" sym))))
              (buf (car loc)))
         (with-current-buffer buf
           (goto-char (cdr loc))
           (beginning-of-defun)
           (let ((beg (point)))
             (end-of-defun)
             (concat
              (when buffer-file-name
                (format "Defined in %s\n\n" (abbreviate-file-name buffer-file-name)))
              (buffer-substring-no-properties beg (point))))))
     (error (format "Could not read source of %S (%s)" symbol-name err))))
 :description
 "Return the Emacs Lisp source code defining an interactive command,
function or variable. Useful for answering questions about behavior
beyond the docstring."
 :args
 (list '(:name "symbol_name"
               :type string
               :description "Name of the function or variable whose source to return"))
 :category "emacs")

(gptel-make-tool
 :name "search_symbols"
 :function
 (lambda (regexp kind)
   (let ((kind (or kind "")) out)
     (mapatoms
      (lambda (sym)
        (and (string-match-p regexp (symbol-name sym))
             (or (string-empty-p kind)
                 (and (string-prefix-p "func" kind) (fboundp sym))
                 (and (string-prefix-p "var" kind) (boundp sym)))
             (push sym out))))
     (setq out (sort out (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
     (if (null out)
         (format "No symbols matching %S." regexp)
       (mapconcat
        (lambda (sym)
          (let ((doc (condition-case nil
                         (documentation-property sym
                          (if (fboundp sym) 'function-documentation
                            'variable-documentation))
                       (error nil))))
            (format "%s%s" sym
                    (if (and doc (> (length doc) 0))
                        (format " -- %s" (car (split-string doc "[\n\r]")))
                      ""))))
        (seq-take out 60)
        "\n"))))
 :description
 "List Emacs Lisp symbols whose names match REGEXP (up to 60 matches),
with one-line summaries. KIND filters: \"funcs\" only commands/functions,
\"vars\" only variables; empty string searches both. Run this before
describe_symbol when the exact name is unknown."
 :args
 (list '(:name "regexp"
               :type string
               :description "Regular expression matched against symbol names")
       '(:name "kind"
               :type string
               :optional t
               :description "\"funcs\", \"vars\" or empty for both"))
 :category "emacs")

;;; Rescued from old llm.el -- modernized where noted

;; Variable inspection: view runtime value (not just docstring)
(gptel-make-tool
 :name "variable_value"
 :function
 (lambda (variable)
   (let ((sym (intern-soft variable)))
     (if (and sym (boundp sym))
         (let ((val (format "%S" (symbol-value sym))))
           (if (> (length val) 10000)
               (format "%s...[%d chars total]" (substring val 0 10000) (length val))
             val))
       (format "%S is not a bound variable." variable))))
 :description
 "Return the printed representation of the current global value of the Emacs
Lisp variable VARIABLE. Use this to inspect actual runtime state, not just
documentation."
 :args
 (list '(:name "variable"
               :type string
               :description "Name of the bound variable"))
 :category "emacs")

;; Info manuals: clean rewrite replacing the buggy info-lookup/manual-node
;; versions from llm.el.
(defun my-gptel--truncate (text limit)
  (if (> (length text) limit)
      (concat (substring text 0 limit)
              (format "\n[TRUNCATED, %d more characters]" (- (length text) limit)))
    text))

(defun my-gptel--read-info-node (manual node)
  (save-window-excursion
    (Info-find-node manual node)
    (my-gptel--truncate (buffer-string) 20000)))

(gptel-make-tool
 :name "read_manual_node"
 :function
 (lambda (manual node)
   (condition-case err
       (my-gptel--read-info-node manual node)
     (error (format "Could not open node %S in manual %S (%s)"
                    node manual (error-message-string err)))))
 :description
 "Return the full text of NODE in an installed Emacs INFO MANUAL. Common
manuals: \"emacs\", \"elisp\", \"magit\". Start at node \"Top\" when unsure,
or use list_manual_menu first. Prefer this over guessing about behavior."
 :args
 (list '(:name "manual"
               :type string
               :description "Manual name, e.g. \"elisp\" or \"emacs\"")
       '(:name "node"
               :type string
               :description "Node name, e.g. \"Top\" or \"Completion Commands\""))
 :category "emacs")

(defun my-gptel--top-menu-items (manual)
  (save-window-excursion
    (Info-find-node manual "Top")
    (goto-char (point-min))
    (let ((items nil))
      (while (re-search-forward
              "^[ \\t]*\\*[ \\t]+\\([^\\t\\n:]+[^\\t\\n: ]\\)[ \\t]*\\(::\\|:\\)"
              nil t)
        (push (match-string 1) items))
      (mapconcat #'identity (nreverse items) " | "))))

(gptel-make-tool
 :name "list_manual_menu"
 :function
 (lambda (manual)
   (condition-case err
       (my-gptel--top-menu-items manual)
     (error (format "No manual named %S (%s)"
                    manual (error-message-string err)))))
 :description
 "List the chapter/topic names available at the start of an Emacs info
MANUAL. Returns a pipe-separated list usable as node names for
read_manual_node."
 :args
 (list '(:name "manual"
               :type string
               :description "Manual name, e.g. \"elisp\", \"emacs\", \"org\""))
 :category "emacs")

;; Web lookup (trimmed from old fetch/curl pair)
(defun my-gptel--fetch-url (url)
  (let ((buf (url-retrieve-synchronously url)))
    (unless buf
      (error "Failed to retrieve %s" url))
    (with-current-buffer buf
      (goto-char (point-min))
      (re-search-forward "^$" nil t)   ; skip HTTP headers
      (prog1
          (decode-coding-region (point) (point-max) 'utf-8 t)
        (kill-buffer)))))

(gptel-make-tool
 :name "fetch_url"
 :function
 (lambda (url)
   (my-gptel--truncate (my-gptel--fetch-url url) 20000))
 :description "Fetch the content of a public URL as plain text."
 :args
 (list '(:name "url"
               :type string
               :description "The http(s) URL to fetch"))
 :category "web")

;;; Workflow utils rescued from llm.el

(defun my-gptel-context-add-files (beg end)
  "Add each line between BEG and END as a gptel context file.
Works on any buffer; put file paths on their own lines first."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (gptel-context-add-file (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position)))
      (forward-line 1))))

(defun my-gptel-context-add-marked-buffers (&optional arg)
  "Add marked ibuffer buffers (or ARG/current-line buffer) to gptel context.
Bound to C-c g in ibuffer."
  (interactive "P")
  (let ((buffers (or (and arg (list (ibuffer-current-buffer)))
                     (ibuffer-get-marked-buffers))))
    (if buffers
        (progn
          (dolist (buf buffers)
            (with-current-buffer buf
              (gptel-context--add-region buf (point-min) (point-max) t)))
          (message "Added %d buffer%s as gptel context."
                   (length buffers) (if (= 1 (length buffers)) "" "s"))
          (ibuffer-redisplay t))
      (user-error "No marked buffers, and no buffer under point"))))

(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "C-c g") #'my-gptel-context-add-marked-buffers))

;;; Quality-of-life defaults (per gptel README recommendations)

(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
(add-hook 'gptel-post-response-functions 'gptel-end-of-response)

;; Show request inspection options (see exactly what gets sent)
(setq gptel-expert-commands t)

(provide 'gptel-config)
;;; gptel.el ends here
