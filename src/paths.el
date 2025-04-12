;;; paths.el --- Description -*- lexical-binding: t; -*-
(message "paths.el load start")

;; ref: http://turingmachine.org/bl/2013-05-29-recursively-listing-directories-in-elisp.html
(defun ub/directory-files-recursively (directory match maxdepth ignore)
  "List files in DIRECTORY and in its sub-directories.
   Return files that match the regular expression MATCH but ignore
   files and directories that match IGNORE (IGNORE is tested before MATCH. Recurse only
   to depth MAXDEPTH. If zero or negative, then do not recurse"
  (let* ((files-list '())
         (current-directory-list
          (directory-files directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (let ((f (car current-directory-list)))
        (cond
         ((and
           ;; make sure it is not nil
           ;; string match only with file not full path
           ignore (string-match ignore (file-name-nondirectory f))) nil)
         ((and
           (file-regular-p f)
           (file-readable-p f)
           (string-match match (file-name-nondirectory f)))
          (setq files-list (cons f files-list)))
         ((and
           (file-directory-p f)
           (file-readable-p f)
           (not (string-equal ".." (substring f -2)))
           (not (string-equal "." (substring f -1)))
           (> maxdepth 0))
          ;; recurse only if necessary
          (setq files-list (append files-list (ub/directory-files-recursively f match (- maxdepth -1) ignore))))
         (t)))
      (setq current-directory-list (cdr current-directory-list)))
    files-list))

(defun define-dir-path-const (name path)
  (unless (file-directory-p path)
    (message "Warning: Directory path does not exist: %s" path))
  (eval `(defconst ,name ,path "Defines a path constant.")))

(defun define-file-path-const (name path)
  (unless (file-exists-p path)
    (message "Warning: File path does not exist: %s" path))
  (eval `(defconst ,name ,path "Defines a file path constant.")))


(define-dir-path-const 'ub/org-root-dir (file-truename "~/main/org"))
(define-dir-path-const 'ub/gtd-root-dir (file-truename "~/main/org/gtd"))



(message "paths.el load end")

;;; paths.el ends here
