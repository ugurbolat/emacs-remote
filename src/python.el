

(with-eval-after-load 'python
  (let ((map-var python-mode-map))
    ;;(define-key map-var (kbd "C-c C-s") #'quickrun-shell)
    (define-key map-var (kbd "<tab>") 'python-indent-shift-right)
    (define-key map-var (kbd "S-<tab>") 'python-indent-shift-left)
    (define-key map-var [S-iso-lefttab] 'python-indent-shift-left)
    (define-key map-var (kbd "C-c C-i") 'pyimport-insert-missing)
    (define-key map-var (kbd "C-c C-b") 'python-black-region)))

;; add environment variable
;; we modify environment variables for dev. mode
(with-eval-after-load 'python
  (setq python-shell-process-environment
	'(
	  ;; disabling breakpoints
	  ;; to ignore breakpoints not to go into pdb mode
	  ;; python's repl is used for running uninterrupted
	  ;; if you want debugging, call explicitly pdb
	  "PYTHONBREAKPOINT=\"0\""
	  ;; disabling jax's gpu memory preallocation which %90 of the mem.
	  "XLA_PYTHON_CLIENT_PREALLOCATE=\"false\""  
	  )
        ))


(use-package pyvenv
  :ensure (:fetcher github :repo "jorgenschaefer/pyvenv")
  :defer t
  :config
  (setenv "WORKON_HOME" "/home/bolatu/miniconda3/envs")
  ;; TODO
  ;; (pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                ;; remove tramp prefix from pyvenv-virtual-env due to ssh or docker which starts with /ssh: or /docker: and ends with :/
                (setq pyvenv-virtual-env (replace-regexp-in-string "/.*:" "" pyvenv-virtual-env))
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3"))
                ;;(setq doom-modeline-env-python-executable (concat pyvenv-virtual-env "bin/python3"))
                ;;(setq realgud--ipdb-command-name (concat pyvenv-virtual-env "bin/python -m ipdb"))
                ;; (setq realgud:pdb-command-name "pyth on -m pdb")
                (setq realgud:pdb-command-name (concat pyvenv-virtual-env "bin/python -m pdb")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")
                ;;(setq doom-modeline-env-python-executable "python3") 
                (setq realgud:pdb-command-name "python -m pdb")))))


(use-package pdb-capf
  :ensure (:fetcher github :repo "ugurbolat/emacs-pdb-capf")
  :defer 0.1
  :config
  (add-hook 'pdb-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions
                        'pdb-capf nil t)))
  (add-hook 'pdb-track-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions
                        'pdb-capf nil t))))



(defun ub/python-imports-at-current-file-w-grep ()
  "Show Python imports using grep mode."
  (interactive)
  (when-let ((file (buffer-file-name)))
    (let ((grep-command
           (format "rg -U --multiline-dotall --line-number --no-heading --with-filename\
 '^\\s*(from\\s+\\S+\\s+import\\s+\\((?:[^()]*|\\n)*\\)|from\\s+\\S+\\s+import\\s+\\S+|import\\s+\\S+)'\
 %s"
                   (shell-quote-argument file))))
      (grep grep-command))))

(defun ub/python-imports-at-current-dir-w-grep ()
  "Search for Python import statements in the current directory using ripgrep."
  (interactive)
  (let ((grep-command
         "rg -U --multiline-dotall --line-number --no-heading --with-filename\
 '^\\s*(from\\s+\\S+\\s+import\\s+\\((?:[^()]*|\\n)*\\)|from\\s+\\S+\\s+import\\s+\\S+|import\\s+\\S+)'\
 --glob '*.py' ."))
    (grep grep-command)))

(defun ub/python-commented-breakpoints-at-buffer-w-grep ()
  "Find commented-out breakpoints in the current buffer using ripgrep."
  (interactive)
  (when-let ((file (buffer-file-name)))
    (let ((grep-command
           (format "rg -U --multiline-dotall --line-number --no-heading --with-filename\
 '^\\s*#\\s*breakpoint\\s*()?'\
 %s"
                   (shell-quote-argument file))))
      (grep grep-command))))

(defun ub/python-commented-breakpoints-at-dir-w-grep ()
  "Find commented-out breakpoints in the current buffer using ripgrep."
  (interactive)
  (let ((grep-command
         "rg -U --multiline-dotall --line-number --no-heading --with-filename\
 '^\\s*#\\s*breakpoint\\s*()?'\
 --glob '*.py' ."))
    (grep grep-command)))

(defun ub/python-uncommented-breakpoints-at-buffer-w-grep ()
  "Find uncommented-out breakpoints in the current buffer using ripgrep."
  (interactive)
  (when-let ((file (buffer-file-name)))
    (let ((grep-command
           (format "rg -U --multiline-dotall --line-number --no-heading --with-filename\
 '^[^#]*\\bbreakpoint\\s*()?'\
 %s"
                   (shell-quote-argument file))))
      (grep grep-command))))

(defun ub/python-uncommented-breakpoints-at-dir-w-grep ()
  "Find uncommented-out breakpoints in the current directory using ripgrep."
  (interactive)
  (let ((grep-command
         "rg -U --multiline-dotall --line-number --no-heading --with-filename\
 '^[^#]*\\bbreakpoint\\s*()?'\
 --glob '*.py' ."))
    (grep grep-command)))


(defun ub/python-summary-at-buffer-w-grep ()
  (interactive)
  (when-let ((file (buffer-file-name)))
    (let ((grep-command
           (format "rg -U --multiline-dotall --line-number --no-heading --with-filename\
 '^\\s*(def|class)\\s+\\w+\\s*\\([^)]*\\)?:?'\
 %s"
                   (shell-quote-argument file))))
      (grep grep-command))))

(defun ub/python-summary-at-dir-w-grep ()
  (interactive)
  (let ((grep-command
         "rg -U --multiline-dotall --line-number --no-heading --with-filename\
 '^\\s*(def|class)\\s+\\w+\\s*\\([^)]*\\)?:?'\
 --glob '*.py' ."))
    (grep grep-command)))



(defun ub/python-uncommented-breakpoint-at-dir-w-consult()
  (interactive)
  (let ((consult-async-split-style nil))
    (ub/consult-ripgrep-dir "^[^#]*breakpoint()")))

(defun ub/python-commented-breakpoint-at-dir-w-consult ()
  (interactive)
  (let ((consult-async-split-style nil))
    (ub/consult-ripgrep-dir "^[^#]*#.*breakpoint()")))

(defun ub/python-uncommented-breakpoint-at-buffer-w-consult()
  (interactive)
  (let ((consult-async-split-style nil))
    (ub/consult-ripgrep-buffer "^[^#]*breakpoint()")))

(defun ub/python-commented-breakpoint-at-buffer-w-consult ()
  (interactive)
  (let ((consult-async-split-style nil))
    (ub/consult-ripgrep-buffer "^[^#]*#.*breakpoint()")))


(defun ub/python-summary-at-dir-w-consult ()
  (interactive)
  (let ((consult-async-split-style nil)) ;; disables default behavior of placing # in front
    (ub/consult-ripgrep-dir "\\<def\\>\\|\\<class\\>")))

(defun ub/python-summary-at-buffer-w-consult ()
  (interactive)
  (let ((consult-async-split-style nil))
    (ub/consult-ripgrep-buffer "\\<def\\>\\|\\<class\\>")))



(defun ub/python-imports-at-dir-w-consult ()
  (interactive)
  (let ((consult-async-split-style nil))
    (ub/consult-ripgrep-dir "^\\s*\\(from\\|import\\)\\s+")))
(defun ub/python-imports-at-buffer-w-consult ()
  (interactive)
  (let ((consult-async-split-style nil)
        (consult-ripgrep-args "rg --multiline --multiline-dotall")
        )
    (ub/consult-ripgrep-buffer "^\\s*\\(from\\|import\\)\\s+")))




;; TODO
;; (defun ub/python-search-uncommented-breakpoint-w-wgrep()
;;   (interactive)
;;   (ub/consult-ripgrep-current-dir "breakpoint()#^[^#]*breakpoint()")
;;   (embark-export)
;;   (wgrep-change-to-wgrep-mode)
;;   )


(defun python-remove-unused-imports ()
  "Remove unused imports from current Python buffer using autoflake."
  (interactive)
  (when (and buffer-file-name
             (eq major-mode 'python-mode))
    (let ((command (format "autoflake --in-place --remove-all-unused-imports %s"
                           (shell-quote-argument buffer-file-name))))
      (shell-command command)
      (revert-buffer t t t))))
