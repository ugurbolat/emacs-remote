;;; src/remote-runner-args-python-transient.el -*- lexical-binding: t; -*-

(require 'transient)

(defun transient--generate-unique-key (name used-keys)
  "Generate a unique single-letter key from NAME, avoiding USED-KEYS."
  (let* ((clean-name (replace-regexp-in-string "^--" "" name))
         (words (split-string clean-name "-"))
         (chars (mapcar (lambda (word) (downcase (substring word 0 1))) words))
         (key (catch 'found
                ;; Try first letter of each word
                (dolist (c chars)
                  ;;(message "Trying key %s for %s (used: %S)" c name used-keys) ;; Debug
                  (unless (member c used-keys)
                    (throw 'found c)))
                ;; Try each letter in the name
                (let ((all-chars (mapconcat 'identity words "")))
                  (dotimes (i (length all-chars))
                    (let ((c (downcase (substring all-chars i (1+ i)))))
                      ;;(message "Trying letter %s from %s (used: %S)" c name used-keys) ;; Debug
                      (unless (member c used-keys)
                        (throw 'found c)))))
                ;; Fallback to numbered keys if necessary
                (let ((i 1))
                  (while t
                    (let ((numbered-key (format "%d" i)))
                      (unless (member numbered-key used-keys)
                        (throw 'found numbered-key)))
                    (setq i (1+ i)))))))
    ;;(message "Generated key %s for %s" key name) ;; Debug
    key))

(defun transient--make-python-suffix (arg used-keys)
  "Convert ARG into a transient suffix specification, using USED-KEYS to track taken keys."
  (let* ((name (alist-get 'name arg))
         (clean-name (replace-regexp-in-string "^--" "" name))
         (type (or (alist-get 'type arg) "str"))
         (help (or (alist-get 'help arg) "No description available"))
         (required (eq t (alist-get 'required arg)))
         (default (alist-get 'default arg))
         (choices (alist-get 'choices arg))
         (key (transient--generate-unique-key name used-keys))
         (description (format "%s %s%s%s%s"
                              help
                              (propertize (format "[%s]" type) 'face 'font-lock-type-face)
                              (if required
                                  (propertize " (required)" 'face 'error)
                                "")
                              (if default
                                  (format " (default: %s)"
                                          (propertize (format "%s" default)
                                                      'face 'font-lock-constant-face))
                                "")
                              (if choices
                                  (format " (choices: %s)"
                                          (propertize (format "%s"
                                                              (if (vectorp choices)
                                                                  (mapconcat #'identity (append choices nil) ", ")
                                                                choices))
                                                      'face 'font-lock-builtin-face))
                                "")))
         (cmd (intern (format "transient-python-arg%s"
                              (replace-regexp-in-string "-" "" name)))))
    `(,cmd
      :class transient-option
      :description ,description
      :argument ,(format "%s=" name)
      :key ,key
      :prompt ,(format "%s (%s%s): " help type (if required " required" ""))
      :reader ,(cond
                ;; For choices, use completing-read
                (choices
                 `(lambda (prompt _initial _history)
                    (completing-read prompt
                                     ',(if (vectorp choices)
                                           (append choices nil)
                                         (split-string choices "," t " "))
                                     nil t)))
                ;; For boolean flags (store_true action), use y-or-n-p
                ((string= type "bool")
                 'transient-read-boolean-or-value)
                ;; Default to read-string
                (t #'read-string)))))

(defun transient--group-python-args (args)
  "Group ARGS into required and optional arguments."
  (let ((used-keys (make-hash-table :test 'equal))
        required optional)
    (dolist (arg args)
      (let* ((suffix (transient--make-python-suffix arg (hash-table-keys used-keys))))
        (let ((key (plist-get (cdr suffix) :key)))
          (puthash key (alist-get 'name arg) used-keys))
        (if (alist-get 'required arg)
            (push suffix required)
          (push suffix optional))))
    `([ "Required Arguments"
        ,@(nreverse required)]
      [ "Optional Arguments"
        ,@(nreverse optional)])))

(defun transient-define-python-cli-prefix (args)
  "Define transient prefix for Python CLI ARGS."
  (let* ((base-name (or (file-name-base (buffer-file-name))
                        (buffer-name)))
         (cmd-name (intern (format "transient-python-cli-%s" base-name)))
         (groups (transient--group-python-args args)))
    ;;(message "Defining command %S with groups: %S" cmd-name groups) ;; Debug print
    (eval
     `(progn
        (transient-define-prefix ,cmd-name ()
          "Python CLI Arguments"
          [:description
           (lambda () (format "Arguments for: %s" (buffer-name)))]
          ,@groups
          [["Actions"
            ("RET" "Execute with args" transient--python-cli-done)
            ("C-g" "Abort" transient-quit-all)]])
        ',cmd-name))))


(defun transient-setup-python-args ()
  "Parse current buffer and setup transient menu."
  (interactive)
  (let ((args (remote-runner-parse-python-argparse (current-buffer))))
    (message "Parsed args: %S" args) ;; Debug print
    (if args
        (let ((cmd (transient-define-python-cli-prefix args)))
          ;;(message "Generated command: %S" cmd) ;; Debug print
          (if (fboundp cmd)
              (progn
                (message "Calling command %S" cmd) ;; Debug print
                (call-interactively cmd))
            (error "Transient command %s not defined" cmd)))
      (message "No Python CLI arguments found in buffer"))))

(defun transient--python-cli-done ()
  "Collect values from transient and execute the command."
  (interactive)
  (let* ((args (transient-args transient-current-command))
         (formatted-args (mapcar (lambda (arg)
                                   (if (string-match "\\(.*\\)=\\(.*\\)" arg)
                                       (format "%s %s"
                                               (match-string 1 arg)
                                               (match-string 2 arg))
                                     arg))
                                 args))
         (args-string (string-join formatted-args " ")))
    (transient-quit-all)

    ;; Update config with new args
    (let ((config remote-runner-config))
      (setf (alist-get 'python-args config) args-string)

      ;; Execute with the new args
      (remote-runner-execute-with-args args-string))))

(provide 'remote-runner-args-python-transient)
