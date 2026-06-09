;;; -*- lexical-binding: t; -*-

(load "secrets.el")

(use-package gptel
  :ensure t
  :pin melpa
  :config
  (add-hook 'gptel-mode-hook #'visual-line-mode)
  (add-hook 'gptel-mode-hook
            (lambda ()
              ;; (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
              ;; (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll nil t)
              ))
  (gptel-make-azure "azure-arcline-gpt-5"
                 :protocol "https"
                 :host "caseiai-playground.openai.azure.com"
                 :endpoint "/openai/deployments/gpt-5/chat/completions?api-version=2025-01-01-preview"
                 :stream t
                 :key arcline-azure
                 :models '(gpt-5))
  (gptel-make-anthropic "azure-arcline-claude"
                 :protocol "https"
                 :host "caseiai-playground.services.ai.azure.com"
                 :endpoint "/anthropic/v1/messages"
                 :stream t
                 :key arcline-azure
                 :models '(claude-sonnet-4-5-20250929))
  (gptel-make-anthropic "chat: claude"          ;Any name you want
    :stream t                             ;Streaming responses
    :key claude-key)
  (gptel-make-gemini "chat: gemini" :key gemini-key :stream t)
  ;; DeepSeek offers an OpenAI compatible API
  (gptel-make-openai "chat: deepseek"       ;Any name you want
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key deepseek-key
    :models '(deepseek-chat deepseek-coder deepseek-reasoner))




(global-set-key (kbd "C-<tab>") 'gptel-send)

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "*** USER: ")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "**** LLM\n")
  ;; OpenRouter offers an OpenAI compatible API
  (gptel-make-openai "chat: openrouter"               ;Any name you want
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key openrouter-key                   ;can be a function that returns the key
    :models '(openai/gpt-5
	      openai/gpt-5-mini
	      ))
  (gptel-make-openai "chat: openrouter-non-reasoning"               ;Any name you want
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key openrouter-key                   ;can be a function that returns the key
    :models '(openai/gpt-5
	      openai/gpt-5-mini)
    :request-params '(:reasoning (:effort "low")))

  (setq
   transient-show-during-minibuffer-read t
   gptel-org-branching-context nil
   gptel-log-level 'info
   gptel-model 'claude-sonnet-4-20250514
   gptel-api-key openai-key
   gptel-default-mode 'org-mode)
  (load "llm-prompts"))


(use-package mcp
  :ensure t
  :pin melpa
  :after gptel
  :custom (mcp-hub-servers
           `(("filesystem" .
	      (:command "npx" :args
			("-y" "@modelcontextprotocol/server-filesystem"
			 "/Users/vegardok/tmp/"
			 "/Users/vegardok/repos/spt/terraform-monitoring/datafoundations/alerts/teams/kollectorsk8s"
			 "/Users/vegardok/repos/spt/kollector-spillover/"
			 )))
             ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
	     ("brave" . (:command "npx"
				  :args ("-y" "@modelcontextprotocol/server-brave-search")
				  :env (:BRAVE_API_KEY "BSA6F1mziNGa_O7lWsoSQSTKxt5wX9O" )))
	     ("sequential-thinking" . (:command "npx"
						:args ("-y" "@modelcontextprotocol/server-sequential-thinking")))
	     ))
  :config (require 'mcp-hub)
  :hook (after-init . mcp-hub-start-all-server))

(load-file "~/repos/dotfiles/gptel-integrations.el")
(require 'gptel-integrations)



(defun v/gptel-context-add-files (beg end)
  "Call `gptel-context-add-file' for each line in the region from BEG to END."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))
        (gptel-context-add-file line))
      (forward-line 1))))

;; Consider using `mapc` or `dolist` instead of `while` for better readability
;; Check if `gptel-context-add-file` exists before calling it
;; Add error handling for invalid file names or non-existent files

(require 'ibuffer)
(require 'gptel-context)

(defun v/gptel-ubuffer-add (buffer)
  "Add the entire BUFFER as a gptel context."
  (with-current-buffer buffer
    (gptel-context--add-region buffer (point-min) (point-max) t)))

;; Consider adding a check to ensure `buffer` is valid before proceeding

(defun v/ibuffer-gptel-operation (&optional arg)
  "Add marked buffers as gptel contexts in ibuffer.
  If ARG is non-nil, operate on ARG (or current) line's buffer."
  (interactive "P")
  (let ((buffers (or (and arg (list (ibuffer-current-buffer)))
                     (ibuffer-get-marked-buffers))))
    (if buffers
        (progn
          (dolist (buf buffers)
            (v/gptel-ubuffer-add buf))
          (message "Added %d buffer%s as gptel context."
                   (length buffers)
                   (if (= 1 (length buffers)) "" "s"))
          (ibuffer-redisplay t))
      (error "No buffers marked, not operating on current buffer"))))

;; Consider using `user-error` instead of `error` for non-programming errors
;; Add a check to ensure `gptel-context--add-region` is available

(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "C-c g") 'v/ibuffer-gptel-operation))

;; Consider using `define-key` with `use-package` for better organization


;; TOOLS





(defun fetch (url)
  (condition-case err
      (with-current-buffer (url-retrieve-synchronously url)
        (goto-char (point-min))
        (re-search-forward "^$" nil t) ; Skip headers
        (buffer-substring-no-properties (point) (point-max)))
    (error (format "Failed to fetch URL: %s" (error-message-string err)))))


(gptel-make-tool
 :name "fetch_url"
 :function (lambda (url)
             (message "gptel: fetch_url: %s" url)
             (fetch url))
 :description "Fetch and return the contents of a URL"
 :args (list '(:name "url"
                     :type string
                     :description "The URL to fetch"))
 :category "web")


(defun my-curl (url &optional args method)
  "Run curl command on URL with optional ARGS (a list of strings).
Add -v and -s options by default.
If METHOD is non-nil, add `-X METHOD` to the arguments.
Return the output as a string.

Example:
  (my-curl \"http://example.com\" '(\"-I\") \"HEAD\")  ; fetch headers only using HEAD method"

  (let* ((default-args '("-v" "-s"))
         (method-arg (when method (list "-X" method)))
         (cmd-args (append default-args method-arg args (list url)))
         (cmd (mapconcat #'shell-quote-argument (cons "curl" cmd-args) " "))
         (output-buffer "*my-curl-output*"))
    ;; Call shell-command and capture output in buffer
    (with-output-to-string
      (with-current-buffer standard-output
        (erase-buffer)
        (call-process-shell-command cmd nil t)
        (buffer-string)))))


(gptel-make-tool
 :name "curl_request"
 :function (lambda (url &optional args method)
             (message "gptel: curl_request: %s %s %s" url args method)

             (my-curl url args method))
 :description "Perform a curl HTTP request to a URL with optional additional args and method."
 :args (list
        '(:name "url" :type string :description "The URL to send the request to")
        '(:name "args" :type (list string) :description "Optional list of additional curl arguments")
        '(:name "method" :type string :description "Optional HTTP method to set with -X"))
 :category "network")


(gptel-make-tool
 :name "variable_completions"
 :function (lambda (prefix)
             (message "gptel: variable_completions: %s" prefix)
             (let (results)
               (mapatoms (lambda (sym)
                           (when (and (boundp sym)
                                      (string-prefix-p prefix (symbol-name sym)))
                             (push (symbol-name sym) results))))
               (nreverse results)))
 :description "Provide a list of all Emacs Lisp variables whose names start with the given PREFIX string. This tool is designed for Emacs Lisp introspection, helping to discover available variables that match a partial or complete prefix."
 :args (list '(:name "prefix" :type string :description "Prefix of variable names"))
 :category "introspection")


(gptel-make-tool
 :name "command_completions"
 :function (lambda (prefix)
             (message "gptel: command_completions: %s" prefix)
             (let (results)
               (mapatoms (lambda (sym)
                           (when (and (commandp sym)
                                      (string-prefix-p prefix (symbol-name sym)))
                             (push (symbol-name sym) results))))
               (nreverse results)))
 :description "Given a string PREFIX, returns a list of all Emacs command names that start with PREFIX. This tool helps to find available command functions for autocompletion, command discovery, or meta-programming in Emacs."
 :args (list '(:name "prefix" :type string :description "Prefix of command names"))
 :category "introspection")


(gptel-make-tool
 :name "function_completions"
 :function (lambda (prefix)
             (message "gptel: function_completions: %s" prefix)
             (let (results)
               (mapatoms (lambda (sym)
                           (when (and (fboundp sym)
                                      (string-prefix-p prefix (symbol-name sym)))
                             (push (symbol-name sym) results))))
               (nreverse results)))
 :description "Given a prefix string, returns a list of Emacs Lisp function names that start with the provided prefix. Useful for discovering available functions and for auto-completion tasks within the Emacs environment."
 :args (list '(:name "prefix" :type string :description "Prefix of function names"))
 :category "introspection")


(gptel-make-tool
 :name "variable_documentation"
 :function (lambda (variable)
             (message "gptel: variable_documentation: %s" variable)
             (let ((sym (intern-soft variable)))
               ;; Check if sym is bound before trying to get documentation
               (when (and sym (boundp sym))
                 (or (documentation-property sym 'variable-documentation)
                     (documentation-property sym 'custom-documentation)
                     "No documentation found."))))
 :description "Retrieve the detailed documentation string for a given Emacs Lisp variable name, prioritizing standard and custom documentation properties. Returns a helpful message if the variable is unbound or undocumented."
 :args (list '(:name "variable" :type string :description "Name of the variable"))
 :category "introspection")


(gptel-make-tool
 :name "function_documentation"
 :function (lambda (function)
             (message "gptel: function_documentation: %s" function)
             (let ((sym (intern-soft function)))
               (when (and sym (fboundp sym))
                 ;; Using `documentation` function from Emacs to get function doc string
                 (or (documentation sym) "No documentation found."))))
 :description "Fetches the documentation string for a given Emacs Lisp function name; returns the doc string if available or a message stating none is found. This tool helps LLM agents by providing accurate function usage and behavioral descriptions from Emacs Lisp environment."
 :args (list '(:name "function" :type string :description "Name of the Emacs Lisp function whose documentation is to be retrieved"))
 :category "introspection")


(gptel-make-tool
 :name "variable_value"
 :function (lambda (variable)
             (message "gptel: variable_value: %s" variable)
             (let ((sym (intern-soft variable)))
               ;; Ensure the symbol exists and is bound before getting value
               (when (and sym (boundp sym))
                 (format "%S" (symbol-value sym)))))
 :description "Fetches and returns the global value of a given Emacs Lisp variable named VARIABLE, if it exists and is bound. Returns the printed representation of the variable's value as a string. Use this to inspect the current global value of any Emacs Lisp variable by specifying its name as a string."
 :args (list '(:name "variable" :type string :description "Name of the variable"))
 :category "introspection")


(gptel-make-tool
 :name "variable_source"
 :function (lambda (variable)
             (message "gptel: variable_source: %s" variable)
             (let ((sym (intern-soft variable)))
               (when (and sym (boundp sym))
                 ;; find-variable-noselect returns (BUFFER . POSITION), so src holds this pair
                 (let ((src (find-variable-noselect sym)))
                   (when src
                     (with-current-buffer (car src)
                       (buffer-substring-no-properties (point-min) (point-max))))))))
 :description "Fetches and returns the complete source code defining the Emacs Lisp variable named VARIABLE, or nil if the variable is not defined."
 :args (list '(:name "variable" :type string :description "Name of the Emacs Lisp variable"))
 :category "introspection")


(gptel-make-tool
 :name "function_source"
 :function (lambda (function)
             (message "gptel: function_source: %s" function)
             (let* ((sym (intern-soft function))
                    (posn (and sym (condition-case nil
                                       (find-function-noselect sym)
                                     (error nil)))))
               ;; If function is a byte-compiled or if source location not found, this may fail
               (if (and posn (bufferp (car posn)) (numberp (cdr posn)))
                   (with-current-buffer (car posn)
                     (save-excursion
                       (goto-char (cdr posn))
                       (let ((beg (point))
                             (end (progn
                                    (end-of-defun)
                                    (point))))
                         (buffer-substring-no-properties beg end))))
                 (format "Source code for function '%s' not found or unavailable." function))))
 :description "Retrieve the full source code of an Emacs Lisp function by its name as a string, or return an error message if the source is not found or accessible. Useful for programmatic introspection or analysis."
 :args (list '(:name "function" :type string :description "Name of the Emacs Lisp function to retrieve source code for"))
 :category "introspection")


(gptel-make-tool
 :name "symbol_manual_section"
 :function (lambda (symbol)
             (message "gptel: symbol_manual_section: %s" symbol)
             (let ((manual (when (fboundp 'info-lookup-symbol)
                             ;; info-lookup-symbol returns a manual node or nil; consider handling other types if applicable
                             (info-lookup-symbol symbol 'emacs-lisp-mode))))
               (or manual "No manual entry found.")))
 :description "Fetches and returns the Info manual node contents associated with a given Emacs Lisp SYMBOL, or a message if none is found. Useful for obtaining detailed documentation from Emacs manuals."
 :args (list '(:name "symbol" :type string :description "Name of the Emacs Lisp symbol to look up in the manual"))
 :category "introspection")


(gptel-make-tool
 :name "library_source"
 :function (lambda (library)
             (message "gptel: library_source: %s" library)
             (let ((lib-file (locate-library library)))
               (if (and lib-file (file-readable-p lib-file))
                   (with-temp-buffer
                     (insert-file-contents lib-file)
                     (buffer-string))
                 ;; If the library is not found or unreadable, return an informative message instead of error
                 (format "Library %s not found or unreadable." library))))
 :description "Retrieve the complete source code as a string for a given Emacs Lisp library name. Useful for inspecting or analyzing the implementation details of the specified library."
 :args (list '(:name "library" :type string :description "The exact name of the Emacs Lisp library to load and read its source code"))
 :category "introspection")


(gptel-make-tool
 :name "manual_node_contents"
 :function (lambda (manual node)
             (message "gptel: manual_node_contents: manual=%s node=%s" manual node)
             (with-temp-buffer
               ;; Check if the manual exists, else return an error message
               (if (not (info-library-version manual))
                   (format "Manual %s not found." manual)
                 (info manual nil nil t)
                 (info-goto-node node)
                 (buffer-string))))
 :description "Fetch and return the full text content of a specified NODE within a given Emacs INFO MANUAL. Returns an error string if the manual is missing. Useful for retrieving detailed documentation or reference material from Emacs manuals."
 :args (list '(:name "manual" :type string :description "The name of the Emacs info manual to access")
             '(:name "node" :type string :description "The node title within the manual whose content you want to retrieve"))
 :category "introspection")


(gptel-make-tool
 :name "manual_nodes"
 :function (lambda (manual)
             (message "gptel: manual_nodes: %s" manual)
             ;; The function uses info-get-node, which expects manual as a symbol or string; ensure manual is interpreted correctly.
             (let ((nodes (info-get-node manual t t)))
               (if nodes
                   (mapcar #'car nodes)
                 (format "No nodes found for manual %s." manual))))
 :description "Fetch a list of topic node names from the specified Emacs Info manual. Returns node headings to assist in navigating manual contents."
 :args (list '(:name "manual" :type string :description "Name of the Emacs manual to query"))
 :category "introspection")


(gptel-make-tool
 :name "manual_names"
 :function (lambda ()
             (message "gptel: manual_names")
             ;; Info-directory-contents is expected to be available and up-to-date
             (mapcar #'car Info-directory-contents))
 :description "Fetch and return a list of available Emacs info manual names currently present in the Info directory, useful for discovering documentation manuals."
 :args nil
 :category "introspection")


(gptel-make-tool
 :name "features"
 :function (lambda (feature)
             (message "gptel: features: %s" feature)
             (featurep (intern feature))) ; Assumes feature is a string, interns it and checks if it's loaded
 :description "Return non-nil if FEATURE (a feature name string) is currently loaded or available in Emacs."
 :args (list '(:name "feature" :type string :description "Feature name"))
 :category "introspection")


(gptel-make-tool
 :name "load_paths"
 :function (lambda ()
             (message "gptel: load_paths")
             ;; Returns the current list of directories Emacs searches for loading libraries
             load-path)
 :description "Retrieve the current Emacs 'load-path' variable, which is a list of directories Emacs searches for libraries and files to load. Useful for understanding or modifying the Emacs environment's available packages and scripts."
 :args nil
 :category "introspection")


(gptel-make-tool
 :name "symbol_exists"
 :function (lambda (symbol)
             (message "gptel: symbol_exists: %s" symbol)
             ;; Using intern-soft returns the symbol if it exists, nil otherwise
             ;; This correctly checks for symbol existence without creating new symbols.
             (intern-soft symbol))
 :description "Check if a symbol named SYMBOL exists in Emacs' obarray without creating a new symbol. Returns the symbol if it exists, or nil if it does not."
 :args (list '(:name "symbol" :type string :description "Symbol name as a string to check for existence"))
 :category "introspection")



;; elisp_eval
;; (gptel-make-tool
;;  :name "elisp_eval"
;;  :function (lambda (expression)
;;              ;; Using `read` on user input can be risky if input is untrusted, consider safer parsing if necessary
;;              (condition-case err
;;                  (prin1-to-string (eval (read expression)))
;;                (error (format "Error: %s" err))))
;;  :description "Evaluate a given Emacs Lisp expression provided as a string EXPRESSION, parse it safely, evaluate it within Emacs environment, and return the printed string result or an error message. Use this tool to run Emacs Lisp code snippets and retrieve their output in textual form."
;;  :args (list '(:name "expression" :type string :description "Elisp expression to evaluate"))
;;  :category "introspection"
;;  :confirm t)

(defun gptel-list-buffers-info ()
  "Return a string listing all buffers with info like `buffer-menu`.

Buffers starting and ending with `*` are excluded from the list."
  (let* ((current (current-buffer))
         (buffers (buffer-list))
         (name-width 20)
         (size-width 7)
         (mode-width 15)
         (category-width 10)
         (lines (list (format "%-2s %-2s %-2s %-20s %7s %-15s %-10s %s"
                              "C" "R" "M" "Buffer" "Size" "Mode" "Category" "File"))))
    (dolist (buf buffers)
      (with-current-buffer buf
        (let ((name (buffer-name buf)))
          ;; Skip buffers starting and ending with '*', also those starting with ' ' per buffer-menu.
          (unless (or (string-prefix-p " " name)
                      (and (string-prefix-p "*" name)
                           (string-suffix-p "*" name)))
            (let* ((c (if (eq buf current) "." " "))
                   (r (if buffer-read-only "%" " "))
                   (m (if (buffer-modified-p) "*" " "))
                   (size (number-to-string (buffer-size buf)))
                   (mode (substring-no-properties (format-mode-line mode-name buf)))
                   (file (or (and buffer-file-name (substring-no-properties buffer-file-name)) ""))
                   (category (cond ((buffer-file-name) "file")
                                   ((get-buffer-process buf) "process")
                                   (t "other"))))
              (push (format "%-2s %-2s %-2s %-20s %7s %-15s %-10s %s"
                            c r m name size mode category file)
                    lines))))))
    (string-join (nreverse lines) "\n")))

(gptel-make-tool
 :name "list-buffers"
 :function (lambda ()
             (message "gptel: list-buffers")
             (gptel-list-buffers-info))
 :description "Lists all Emacs buffers and provides details including the buffer-menu command. the output of this command can change frequently behind the scenes so it is a good idea to call it again rather then using old invocations"
 :args nil
 :category "emacs")


(gptel-make-tool
 :name "read_buffer"
 :function (lambda (buffer)
             (message "gptel: read_buffer: %s" buffer)
             (unless (buffer-live-p (get-buffer buffer))
               (error "error: buffer %s is not live." buffer))
             (with-current-buffer  buffer
               (buffer-substring-no-properties (point-min) (point-max))))
 :description "return the contents of an emacs buffer"
 :args (list '(:name "buffer"
                     :type string
                     :description "the name of the buffer whose contents are to be retrieved"))
 :category "emacs")


(gptel-make-tool
 :name "create_buffer"
 :function (lambda (buffer_name content)
             (message "gptel: create_buffer: name=%s content=%s" buffer_name content)
             (with-current-buffer (get-buffer-create buffer_name)
               (erase-buffer)
               (insert content)
               (current-buffer)))
 :description "Create a new Emacs buffer named BUFFER_NAME and insert CONTENT string into it."
 :args (list
        '(:name "buffer_name" :type string :description "Name of the new buffer to create")
        '(:name "content" :type string :description "Content string to insert into the buffer"))
 :category "emacs")

;; (gptel-make-tool
;;  :name "append_to_buffer"
;;  :function (lambda (buffer_name content)
;;              (with-current-buffer (get-buffer-create buffer_name)
;;                (goto-char (point-max))
;;                (insert content)
;;                (current-buffer)))
;;  :description "Append CONTENT string to the end of BUFFER_NAME, creating the buffer if it does not exist."
;;  :args (list
;;         '(:name "buffer_name" :type string :description "Name of the buffer to append to")
;;         '(:name "content" :type string :description "Content string to append to the buffer"))
;;  :category "emacs")

;; (gptel-make-tool
;;  :name "insert_to_buffer"
;;  :function (lambda (buffer_name position content)
;;              (with-current-buffer (get-buffer-create buffer_name)
;;                (goto-char position)
;;                (insert content)
;;                (current-buffer)))
;;  :description "Insert CONTENT string at POSITION (point) in BUFFER_NAME, creating the buffer if it does not exist."
;;  :args (list
;;         '(:name "buffer_name" :type string :description "Name of the buffer to insert into")
;;         '(:name "position" :type integer :description "Position (point) to insert content at")
;;         '(:name "content" :type string :description "Content string to insert"))
;;  :category "emacs")
(gptel-make-tool
 :name "insert_to_buffer_line"
 :function (lambda (buffer_name line_number content)
             (with-current-buffer (get-buffer-create buffer_name)
               (goto-char (point-min))
               (forward-line (1- line_number))
               (insert content)
               (current-buffer)))
 :description "Insert CONTENT string at the beginning of LINE_NUMBER in BUFFER_NAME, creating the buffer if it does not exist."
 :args (list
        '(:name "buffer_name" :type string :description "Name of the buffer to insert into")
        '(:name "line_number" :type integer :description "Line number to insert content at")
        '(:name "content" :type string :description "Content string to insert"))
 :category "emacs")


(gptel-make-tool
 :name "delete_lines"
 :function (lambda (buffer_name start_line end_line)
             (message "gptel: delete_lines: buffer_name=%s start_line=%d end_line=%d" buffer_name start_line end_line)
             (with-current-buffer (get-buffer buffer_name)
               (save-excursion
                 (goto-char (point-min))
                 (forward-line (1- start_line))
                 (let ((start (point)))
                   (forward-line (1+ (- end_line start_line)))
                   (delete-region start (point))))
               (current-buffer)))
 :description "Delete lines from START_LINE to END_LINE in BUFFER_NAME."
 :args (list
        '(:name "buffer_name" :type string :description "Name of the buffer")
        '(:name "start_line" :type integer :description "Start line number to delete")
        '(:name "end_line" :type integer :description "End line number to delete"))
 :category "emacs")


(gptel-make-tool
 :name "edit_buffer_region"
 :function (lambda (buffer_name start_line end_line new_content)
             (message "gptel: edit_buffer_region: buffer_name=%s start_line=%d end_line=%d new_content=%s" buffer_name start_line end_line new_content)
             (with-current-buffer (get-buffer-create buffer_name)
               (save-excursion
                 (goto-char (point-min))
                 (forward-line (1- start_line))
                 (let ((start (point)))
                   (goto-char (point-min))
                   (forward-line end_line)
                   (delete-region start (point))
                   (goto-char start)
                   (insert new_content)))
               (current-buffer)))
 :description "Replace text in BUFFER_NAME from START_LINE to END_LINE (inclusive) with NEW_CONTENT. The NEW_CONTENT string should include any required newline characters to preserve proper buffer formatting, as newlines are not added automatically."
 :args (list
        '(:name "buffer_name" :type string :description "Name of the buffer to edit")
        '(:name "start_line" :type integer :description "Start line number to replace")
        '(:name "end_line" :type integer :description "End line number to replace")
        '(:name "new_content" :type string :description "New content to insert replacing the region"))
 :category "emacs")


(gptel-make-tool
 :name "list_project_files"
 :function (lambda (directory)
             (message "gptel: list_project_files: %s" directory)
             (let ((projectile-project-root (file-name-as-directory (expand-file-name directory))))
               (if (file-directory-p projectile-project-root)
                   (projectile-current-project-files)
                 (error "Directory does not exist or is not a directory: %s" directory))))
 :description "List all files in the project rooted at the given DIRECTORY."
 :args (list '(:name "directory" :type string :description "The directory to list project files for"))
 :category "emacs")


(gptel-make-tool
 :name "open_file_in_buffer"
 :function (lambda (filepath)
             (message "gptel: open_file_in_buffer: %s" filepath)
             (unless (file-name-absolute-p filepath)
               (error "File path must be absolute: %s" filepath))
             (if (file-exists-p filepath)
                 (find-file-noselect filepath)
               (error "File does not exist: %s" filepath)))
 :description "Open a file specified by an absolute FILEPATH into an Emacs buffer without switching to it."
 :args (list '(:name "filepath" :type string :description "Absolute path of the file to open"))
 :category "emacs")
