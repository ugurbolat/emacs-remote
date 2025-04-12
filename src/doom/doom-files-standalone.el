;;; src/doom/doom-files-standalone.el -*- lexical-binding: t; -*-

;;; Commentary:
;;; A collection of standalone interactive file commands inspired/stolen/adapted by Doom Emacs.
;;; Contains functions for deleting, copying, moving,
;;; and removing files from recentf list.
;;;
;;; Dependencies: cl-lib, recentf.

;;; Code:

(require 'cl-lib)
(require 'recentf)

;;;###autoload Command
(defun doom/delete-this-file (&optional path force-p)
  "Delete PATH (defaulting to current file), kill its buffers.
If FORCE-P, delete without confirmation."
  (interactive
   (list (buffer-file-name (buffer-base-buffer))
         current-prefix-arg))
  (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
         (short-path (and path (abbreviate-file-name path))))
    (unless path
      (user-error "Buffer is not visiting any file"))
    (unless (file-exists-p path)
      (error "File doesn't exist: %s" path))
    (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
      (user-error "Aborted"))
    (let ((buf (get-file-buffer path))) ; Get buffer before deleting file
      (unwind-protect
          (progn (delete-file path t) t)
        (if (file-exists-p path)
            (error "Failed to delete %S" short-path)
          (when (and buf (buffer-live-p buf))
            (kill-buffer buf))
          (message "Deleted %S" short-path))))))

;;;###autoload Command
(defun doom/copy-this-file (new-path &optional force-p)
  "Copy current buffer's file to NEW-PATH then open NEW-PATH.
If FORCE-P, overwrite the destination file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Copy file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) 'parents) ; Use 'parents for robustness
    (copy-file old-path new-path (or force-p 1))
    (find-file new-path)
    (message "File copied to %S" (abbreviate-file-name new-path))))

;;;###autoload Command
(defun doom/move-this-file (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH.
If FORCE-P, overwrite the destination file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Move file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (when (directory-name-p new-path)
      (setq new-path (concat new-path (file-name-nondirectory old-path))))
    (make-directory (file-name-directory new-path) 'parents) ; Use 'parents for robustness
    (rename-file old-path new-path (or force-p 1))
    (set-visited-file-name new-path t t)
    (message "File moved to %S" (abbreviate-file-name new-path))))

;;;###autoload Command
(defun doom/remove-recent-file (file)
  "Remove FILE from your =recentf-list=."
  (interactive
   (list (completing-read "Remove recent file: "
                          (lambda (string predicate action)
                            (if (eq action 'metadata)
                                '(metadata
                                  (display-sort-function . identity)
                                  (cycle-sort-function . identity)
                                  (category . file))
                              (complete-with-action
                               action recentf-list string predicate)))
                          nil t)))
  (setq recentf-list (delete (recentf-expand-file-name file) recentf-list))
  (recentf-save-list)
  (message "Removed %S from =recentf-list'" (abbreviate-file-name file)))

(provide 'doom-files-standalone)
;;; src/doom/doom-files-standalone.el ends here
