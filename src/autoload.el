(message "autoload.el load start")

;;;###autoload
(defun ub/run-if-server-active (body-fun)
  "Execute BODY-FUN if an Emacs server is running."
  ;;(message "server is active")
  (when (bound-and-true-p server-process)
    (funcall body-fun)))
;;;###autoload
(defun ub/run-if-else-server-active (body-fun &optional else-fun)
  "Execute BODY-FUN if an Emacs server is running, else execute ELSE-FUN."
  (if (bound-and-true-p server-process)
      (progn
        (message "server is active")
        (funcall body-fun))
    (when else-fun
      (progn
        (message "server is not active")
        (funcall else-fun))
      )))

;;;###autoload
(defun ub/insert-commit-prefix-w-emoji ()
  (interactive)
  (let* ((string-list
          '(
            ":tada: init: "
            ":construction: wip: "
            ":christmas-tree: christmas tree bill (torba yasa)"
            ":bookmark: tag: "
            ":sparkles: feat: "
            ":bug: fix: "
            ":books: docs: "
            ":lipstick: style: "
            ":hammer: refactor: "
            ":rotating_light: test: "
            ":smiling-imp: customize:"
            ":wrench: chore:"
            ":ok_hand: review: "
            ":card_index: meta: "
            ":racehorse: perf: "
            ":white_check_mark: addTest: "
            ":heavy_check_mark: passTest: "
            ":zap: update: "
            ":art: fmt: "
            ":fire: remove: "
            ":truck: move: "
            ":green_heart: ci: "
            ":lock: sec: "
            ":arrow_up: upDep: "
            ":arrow_down: downDep: "
            ":shirt: lint: "
            ":ambulance: hotfix: "
            ":rocket: deploy: "
            ":apple: fixMac: "
            ":penguin: fixLinux: "
            ":checkered_flag: fixWin: "
            ":construction_worker: ciBuild: "
            ":chart_with_upwards_trend: analytics: "
            ":heavy_minus_sign: removeDep: "
            ":heavy_plus_sign: addDep: "
            ":whale: docker: "
            ":twisted_rightwards_arrows: merge: "
            ":hankey: badCode: "
            ":rewind: revert: "
            ":boom: breaking: "
            ))
         (selected-string (completing-read "Select a string: " string-list)))
    (insert selected-string)))


;;;###autoload
(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (call-interactively 'fill-paragraph)))

;;;###autoload
(defun unfill-region (start end)
  "Replace newline chars in region from START to END by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

;;;###autoload
(defun unfill-toggle ()
  "Toggle filling/unfilling of the current region.
Operates on the current paragraph if no region is active."
  (interactive)
  (let (deactivate-mark
        (fill-column
         (if (eq last-command this-command)
             (progn (setq this-command nil)
                    most-positive-fixnum)
           fill-column)))
    (call-interactively 'fill-paragraph)))

;;;###autoload
(defun doom/toggle-line-numbers ()
  "Toggle line numbers.
Cycles through regular, relative and no line numbers. The order depends on what
`display-line-numbers-type' is set to. If you're using Emacs 26+, and
visual-line-mode is on, this skips relative and uses visual instead.
See `display-line-numbers' for what these values mean."
  (interactive)
  (require 'display-line-numbers)
  (defvar doom--line-number-style display-line-numbers-type)
  (let* ((styles `(t ,(if visual-line-mode 'visual 'relative) nil))
         (order (cons display-line-numbers-type (remq display-line-numbers-type styles)))
         (queue (memq doom--line-number-style order))
         (next (if (= (length queue) 1)
                   (car order)
                 (car (cdr queue)))))
    (setq doom--line-number-style next)
    (setq display-line-numbers next)
    (message "Switched to %s line numbers"
             (pcase next
               (`t "normal")
               (`nil "disabled")
               (_ (symbol-name next))))))


;; ;;;###autoload
;; (defun +vterm/here (arg)
;;   "Open a terminal buffer in the current window at project root.

;; If prefix ARG is non-nil, cd into `default-directory' instead of project root.

;; Returns the vterm buffer."
;;   (interactive "P")
;;   (+vterm--configure-project-root-and-display
;;    arg
;;    (lambda()
;;      (require 'vterm)
;;      ;; HACK forces vterm to redraw, fixing strange artefacting in the tty.
;;      (save-window-excursion
;;        (pop-to-buffer "*scratch*"))
;;      (let (display-buffer-alist)
;;        (vterm vterm-buffer-name)))))

;; (defun +vterm--configure-project-root-and-display (arg display-fn)
;;   "Sets the environment variable PROOT and displays a terminal using `display-fn`.

;; If prefix ARG is non-nil, cd into `default-directory' instead of project root.

;; Returns the vterm buffer."
;;   (unless (fboundp 'module-load)
;;     (user-error "Your build of Emacs lacks dynamic modules support and cannot load vterm"))
;;   (let* ((project-root (or (doom-project-root) default-directory))
;;          (default-directory
;;           (if arg
;;               default-directory
;;             project-root)))
;;     (setenv "PROOT" project-root)
;;     (funcall display-fn)))
;;;###autoload
(defun +vterm/here (arg)
  "Open a terminal buffer in the current window at project root.

If prefix ARG is non-nil, cd into `default-directory' instead of project root.

Returns the vterm buffer."
  (interactive "P")
  (+vterm--configure-project-root-and-display
   arg
   (lambda ()
     (require 'vterm)
     ;; HACK forces vterm to redraw, fixing strange artefacting in the tty.
     (save-window-excursion
       (pop-to-buffer "*scratch*"))
     (let (display-buffer-alist)
       (vterm vterm-buffer-name)))))

(defun +vterm--configure-project-root-and-display (arg display-fn)
  "Sets the environment variable PROOT and displays a terminal using `display-fn`.

If prefix ARG is non-nil, cd into `default-directory' instead of project root.

Returns the vterm buffer."
  (unless (fboundp 'module-load)
    (user-error "Your build of Emacs lacks dynamic modules support and cannot load vterm"))
  (let* ((project (project-current))
         (project-root (if project
                           (expand-file-name (project-root project))
                         default-directory))
         (default-directory
          (if arg
              default-directory
            project-root)))
    (setenv "PROOT" project-root)
    (funcall display-fn)))


(message "autoload.el load start")
