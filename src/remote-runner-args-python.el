;;; src/remote-runner-args-python.el -*- lexical-binding: t; -*-

(defvar remote-runner-parser-script "
import ast
import json
import sys
from pathlib import Path

def find_argparse_calls(node, args=None):
    if args is None:
        args = []

    # Look for ArgumentParser initialization
    if isinstance(node, ast.Assign):
        if isinstance(node.value, ast.Call):
            if isinstance(node.value.func, ast.Name) and node.value.func.id == 'ArgumentParser':
                return True

    # Look for add_argument calls
    if isinstance(node, ast.Call):
        if isinstance(node.func, ast.Attribute) and node.func.attr == 'add_argument':
            arg_info = {}

            # Get positional args (argument names)
            for arg in node.args:
                if isinstance(arg, ast.Constant):
                    if arg.value.startswith('-'):
                        arg_info['name'] = arg.value

            # Get keyword arguments
            for kw in node.keywords:
                if kw.arg == 'choices':
                    # Handle choices specially
                    if isinstance(kw.value, ast.List):
                        # For literal lists like ['tiny', 'base', 'large']
                        arg_info['choices'] = [
                            elt.value for elt in kw.value.elts
                            if isinstance(elt, ast.Constant)
                        ]
                    elif isinstance(kw.value, ast.Name):
                        # For variable references
                        arg_info['choices'] = kw.value.id
                elif isinstance(kw.value, ast.Constant):
                    arg_info[kw.arg] = kw.value.value
                elif isinstance(kw.value, ast.Name):
                    arg_info[kw.arg] = kw.value.id
                elif isinstance(kw.value, ast.Call):
                    # Handle cases like type=int, float, etc.
                    if isinstance(kw.value.func, ast.Name):
                        arg_info[kw.arg] = kw.value.func.id

            if 'name' in arg_info:
                args.append(arg_info)

    # Recurse through child nodes
    for child in ast.iter_child_nodes(node):
        find_argparse_calls(child, args)

    return args

def parse_file(file_path):
    with open(file_path, 'r') as f:
        tree = ast.parse(f.read())

    args = find_argparse_calls(tree)
    return args

if __name__ == '__main__':
    file_path = sys.argv[1]
    args = parse_file(file_path)
    print(json.dumps(args))
"
  "Python script for parsing argparse arguments using AST.")

(defun remote-runner-save-parser-script ()
  "Save the Python parser script to a temporary file."
  (let ((temp-file (make-temp-file "argparse-parser-" nil ".py")))
    (with-temp-file temp-file
      (insert remote-runner-parser-script))
    temp-file))

(defun remote-runner-vector-to-list (vec)
  "Convert a vector to a list recursively."
  (if (vectorp vec)
      (mapcar #'remote-runner-vector-to-list (append vec nil))
    (if (and (consp vec) (assoc-string "values" vec))
        (mapcar #'remote-runner-vector-to-list (append (cdr vec) nil))
      vec)))

(defun remote-runner-parse-python-argparse (buffer-or-file)
  "Parse Python file in BUFFER-OR-FILE for argparse arguments using AST.
Returns a list of argument specifications."
  (let* ((file (if (bufferp buffer-or-file)
                   (buffer-file-name buffer-or-file)
                 buffer-or-file))
         (parser-script (remote-runner-save-parser-script))
         (json-string
          (with-temp-buffer
            (call-process "python" nil t nil
                          parser-script
                          file)
            (buffer-string))))
    (when (not (string-empty-p json-string))
      (remote-runner-vector-to-list
       (json-parse-string json-string :object-type 'alist)))))

(defun remote-runner-format-arg-value (arg value)
  "Format VALUE for ARG based on its type and nargs."
  (let ((nargs (alist-get 'nargs arg))
        (type (alist-get 'type arg)))
    (if nargs
        ;; Handle list arguments (nargs specified)
        (let ((values (split-string value)))
          ;; For list arguments, we pass each value separately
          (mapconcat (lambda (val)
                       (shell-quote-argument val))
                     values
                     " "))
      ;; Single argument
      (shell-quote-argument value))))

(defun remote-runner-prompt-for-args (args)
  "Prompt for values of command line arguments in ARGS.
Returns a string of formatted arguments."
  (let ((arg-values nil))
    (dolist (arg args)
      (let* ((arg-name (alist-get 'name arg))
             (help-text (remote-runner-format-arg-help arg))
             (default (alist-get 'default arg))
             (nargs (alist-get 'nargs arg))
             (prompt (format "%s%s\nValue: "
                             help-text
                             (if nargs "\n(For multiple values, separate with spaces)" "")))
             (value (read-string prompt)))
        (unless (string-empty-p value)
          (push (format "%s %s"
                        arg-name
                        (remote-runner-format-arg-value arg value))
                arg-values))))
    (string-join (nreverse arg-values) " ")))

(defun remote-runner-format-arg-help (arg)
  "Format help text for an argument specification ARG."
  (let* ((name (alist-get 'name arg))
         (help-text (or (alist-get 'help arg) "No description available"))
         (type (or (alist-get 'type arg) "str"))
         (default (alist-get 'default arg))
         (required (alist-get 'required arg))
         (choices (alist-get 'choices arg))
         (nargs (alist-get 'nargs arg)))
    (format "%s (%s%s)\nHelp: %s\nDefault: %s\nRequired: %s%s%s"
            name
            type
            (if nargs (format ", multiple values allowed" nargs) "")
            help-text
            (if default (format "%s" default) "None")
            (if required "Yes" "No")
            (if choices
                (format "\nChoices: %s"
                        (if (listp choices)
                            (mapconcat #'identity choices ", ")
                          choices))
              "")
            (if choices "\nMust select from available choices!" ""))))


(defun remote-runner-show-last-args ()
  "Show the last used arguments from the configuration."
  (interactive)
  (let ((args (alist-get 'python-args remote-runner-config)))
    (if (and args (not (string-empty-p args)))
        (message "Current arguments: %s" args)
      (message "No arguments are currently set"))))

(provide 'remote-runner-args-python)
