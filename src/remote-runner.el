;; src/remote-runner.el -*- lexical-binding: t; -*-

;; Configuration variables
(defvar remote-runner-log-buffer "*Remote Runner Log*"
  "Buffer name for remote runner logs.")

(defvar remote-runner-config
  '((host . "asd@asd")
    (local-project-root . "/home/asd/main/dev/asd")
    (remote-project-root . "/home/asd/main/dev/asd")
    ;; Environment setup commands (executed in order)
    (venv-commands . ("source /home/asd/miniconda3/etc/profile.d/conda.sh"
                      "conda activate asd"))
    ;; Environment variables as alist
    (env-vars . ((PYTHONPATH . "/home/asd/main/dev/asd/src/asd/src")
                 (PYTHONBREAKPOINT . "0")
                 (XLA_PYTHON_CLIENT_PREALLOCATE . "false")
                 ;; Add more env vars as needed
                 ;; (WANDB_API_KEY . "your-api-key")
                 ))
    (python-cmd . "python")
    (python-args . "")
    (background . t))
  "Configuration for remote runner.")

(defun remote-runner-format-env-vars (env-vars)
  "Format environment variables from ENV-VARS alist into shell exports."
  (mapconcat (lambda (var)
               (format "export %s=%s"
                       (car var)
                       (shell-quote-argument (cdr var))))
             env-vars
             "\n"))

;; SSH connection testing
(defun remote-runner-test-connection (host)
  "Test SSH connection to HOST."
  (= 0 (shell-command
        (format "ssh -q %s 'exit' 2>/dev/null" host))))

(defun remote-runner-log (message &optional command-p)
  "Log MESSAGE to the remote runner log buffer.
If COMMAND-P is non-nil, format MESSAGE as a command."
  (with-current-buffer (get-buffer-create remote-runner-log-buffer)
    (goto-char (point-max))
    (let ((timestamp (format-time-string "[%Y-%m-%d %H:%M:%S]")))
      (insert (format "%s %s%s\n"
                      timestamp
                      (if command-p "$ " "")
                      message))
      (when command-p
        (insert "\n")))))

;; Log management functions
(defun remote-runner-show-log ()
  "Show the remote runner log buffer."
  (interactive)
  (let ((log-window (get-buffer-window remote-runner-log-buffer)))
    (if log-window
        (select-window log-window)
      (switch-to-buffer-other-window remote-runner-log-buffer))))

(defun remote-runner-clear-log ()
  "Clear the remote runner log buffer."
  (interactive)
  (with-current-buffer (get-buffer-create remote-runner-log-buffer)
    (erase-buffer)
    (remote-runner-log "Log cleared")))

;; Paths
(defun remote-runner-get-relative-path (file-path root-path)
  "Get the relative path of FILE-PATH from ROOT-PATH."
  (let ((relative-path (file-relative-name file-path root-path)))
    (if (string-prefix-p ".." relative-path)
        (error "File %s is not within project root %s" file-path root-path)
      relative-path)))

(defun remote-runner-get-remote-path (local-file config)
  "Convert LOCAL-FILE path to remote path using CONFIG."
  (let* ((local-root (alist-get 'local-project-root config))
         (remote-root (alist-get 'remote-project-root config))
         (relative-path (remote-runner-get-relative-path local-file local-root)))
    (concat remote-root "/" relative-path)))

(defun remote-runner-get-relative-file-path (file-path root-path)
  "Get the relative file path from ROOT-PATH.
Converts '/home/user/project/src/module/file.py' to 'src/module/file.py'"
  (let ((relative-path (remote-runner-get-relative-path file-path root-path)))
    relative-path))

;; Copying current buffer file
(defun remote-runner-copy-file (local-file remote-file host &optional options)
  "Copy LOCAL-FILE to REMOTE-FILE on HOST using rsync with OPTIONS.
Default options are -avP (archive, verbose, partial/progress).
Additional options can be provided as a string."
  (let* ((rsync-options (or options "-avP"))
         (command (format "rsync %s %s %s:%s"
                          rsync-options
                          (shell-quote-argument local-file)
                          host
                          (shell-quote-argument remote-file))))
    (remote-runner-log "Copying file using command:")
    (remote-runner-log command t)
    (let ((exit-code (shell-command command)))
      (unless (= exit-code 0)
        (error "Failed to copy file. Exit code: %d" exit-code)))))

(defun remote-runner-ensure-remote-directory (host dir)
  "Ensure remote directory exists"
  (let ((command (format "ssh %s 'mkdir -p %s'"
                         host
                         (shell-quote-argument dir))))
    (remote-runner-log "Ensuring remote directory exists:")
    (remote-runner-log command t)
    (let ((exit-code (shell-command command)))
      (unless (= exit-code 0)
        (error "Failed to create remote directory. Exit code: %d" exit-code)))))

(defun remote-runner-copy-current-buffer ()
  "Copy current buffer to remote host with improved error handling."
  (interactive)
  (let* ((local-file (buffer-file-name))
         (config remote-runner-config)
         (host (alist-get 'host config))
         (remote-file (remote-runner-get-remote-path local-file config))
         (remote-dir (file-name-directory remote-file)))

    ;; Ensure we have all required values
    (unless local-file
      (error "No file associated with current buffer"))
    (unless host
      (error "No host specified in configuration"))
    (unless remote-file
      (error "Could not determine remote file path"))

    ;; Test SSH connection before proceeding
    (unless (remote-runner-test-connection host)
      (error "Cannot connect to host %s" host))

    ;; Create remote directory
    (remote-runner-ensure-remote-directory host remote-dir)

    ;; Log and perform the copy
    (remote-runner-log (format "Copying %s to %s:%s" local-file host remote-file))
    (remote-runner-copy-file local-file remote-file host)
    (remote-runner-log "Copy completed successfully")))

;; Build cli commands/scripts
(defun remote-runner-build-command (file-path &optional config)
  "Build remote execution command for FILE-PATH using CONFIG."
  (let* ((config (or config remote-runner-config))
         (host (alist-get 'host config))
         (script-content (remote-runner-generate-script file-path config))
         ;; Create command that passes script content through stdin to bash
         (cmd (concat "ssh " host " 'bash -s' << 'EOFREMOTERUNNER'\n"
                      script-content
                      "EOFREMOTERUNNER")))

    (remote-runner-log "Generated execution script:")
    (remote-runner-log script-content t)
    (remote-runner-log "\nFinal command:")
    (remote-runner-log cmd t)
    cmd))

(defvar remote-runner-output-counter 1
  "Counter for generating unique output buffer names.")

(defun remote-runner-get-output-buffer-name ()
  "Generate a unique name for the output buffer.
Returns the first available buffer name in the format *Remote Output-N*."
  (let ((base-name "*Remote Output")
        (counter remote-runner-output-counter)
        buffer-name)
    (setq buffer-name
          (if (= counter 1)
              (concat base-name "*")
            (concat base-name "-" (number-to-string counter) "*")))
    ;; If buffer exists, increment counter and try again
    (while (get-buffer buffer-name)
      (setq counter (1+ counter))
      (setq buffer-name (concat base-name "-" (number-to-string counter) "*")))
    ;; Update the counter for next time
    (setq remote-runner-output-counter (1+ counter))
    buffer-name))

(defun remote-runner-generate-script (file-path config)
  "Generate a bash script for executing FILE-PATH using CONFIG."
  (let* ((remote-file (remote-runner-get-remote-path file-path config))
         (venv-commands (alist-get 'venv-commands config))
         (env-vars (alist-get 'env-vars config))
         (python-cmd (alist-get 'python-cmd config))
         (python-args (alist-get 'python-args config))
         (background (alist-get 'background config))
         (remote-project-root (alist-get 'remote-project-root config))
         (relative-path (remote-runner-get-relative-file-path remote-file remote-project-root))
         (log-dir (concat remote-project-root "/rr-logs"))
         (log-file (concat log-dir "/nohup-"
                           (file-name-base relative-path)
                           "-" (format-time-string "%Y%m%d-%H%M%S")
                           ".log")))

    ;; Generate the script content
    (concat "#!/bin/bash\n\n"
            "# Generated by remote-runner.el\n"
            "# " (format-time-string "%Y-%m-%d %H:%M:%S") "\n\n"

            "# Error handling\n"
            "set -e  # Exit on error\n"
            "set -u  # Error on undefined variables\n\n"

            "# Environment variables\n"
            (remote-runner-format-env-vars env-vars)
            "\n\n"

            "# Environment setup commands\n"
            (mapconcat #'identity venv-commands "\n")
            "\n\n"

            "# Create logs directory if it doesn't exist\n"
            "mkdir -p " log-dir "\n\n"

            "# Change to project directory\n"
            "cd " remote-project-root "\n\n"

            "# Execute Python script\n"
            "echo \"Starting execution of " relative-path "\"\n"
            (if background
                (concat "nohup " python-cmd " -u " relative-path
                        (when (and python-args (not (string-empty-p python-args)))
                          (concat " " python-args))
                        " > " log-file " 2>&1 & \n"
                        "PID=$!\n"
                        "echo \"Process started with PID: $PID\"\n"
                        "echo \"Log file: " log-file "\"\n"
                        "sleep 1  # Give process time to start\n"
                        "if ps -p $PID > /dev/null; then\n"
                        "    echo \"Process is running\"\n"
                        "    tail -f " log-file "\n"
                        "else\n"
                        "    echo \"Process failed to start\"\n"
                        "    cat " log-file "\n"
                        "    exit 1\n"
                        "fi\n")
              (concat python-cmd " " relative-path
                      (when (and python-args (not (string-empty-p python-args)))
                        (concat " " python-args))
                      "\n")))))

(defun remote-runner-display-buffer-other-window (buffer)
  "Display BUFFER in another window without selecting it.
If buffer is already displayed in some window, reuse that window."
  (let ((window (get-buffer-window buffer)))
    (if window
        ;; If buffer is already displayed, just ensure window isn't selected
        (progn
          (set-window-point window (with-current-buffer buffer (point)))
          (set-window-buffer window buffer))
      ;; Otherwise, display in other window without selecting
      (save-selected-window
        (display-buffer buffer
                        '((display-buffer-reuse-window
                           display-buffer-in-previous-window
                           display-buffer-pop-up-window)
                          (inhibit-same-window . t)))))))

(defun remote-runner--default-filter (proc output)
  "Process output from PROC, handling ANSI color codes in OUTPUT."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (let ((start (point)))
            (insert output)
            ;; Apply ANSI color codes to the newly inserted text
            (ansi-color-apply-on-region start (point)))
          (set-marker (process-mark proc) (point)))
        ;; If point was at the end, move it to the new end.
        (when moving (goto-char (process-mark proc)))))))

(defun remote-runner-execute-with-args (args)
  "Execute current buffer with ARGS."
  (let* ((local-file (buffer-file-name))
         (config remote-runner-config)
         (host (alist-get 'host config))
         (output-buffer (remote-runner-get-output-buffer-name)))

    ;; Update config with the args
    (setf (alist-get 'python-args config) args)

    ;; Setup the output buffer
    (with-current-buffer (get-buffer-create output-buffer)
      (erase-buffer)
      (insert (format "Remote execution of %s\n" local-file)
              (format "With arguments: %s\n" args)
              (format "Started at %s\n\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S"))))

    (remote-runner-display-buffer-other-window output-buffer)

    ;; Build and execute command
    (let* ((cmd (remote-runner-build-command local-file config))
           (process (start-process-shell-command
                     "remote-runner"
                     output-buffer
                     cmd)))

      ;; Add process filter for ANSI color handling
      (when (require 'ansi-color nil t)
        (set-process-filter process #'remote-runner--default-filter))

      ;; Add process sentinel to handle completion
      (set-process-sentinel
       process
       (lambda (proc event)
         (when (string-match "\\(finished\\|exited\\)" event)
           (remote-runner-log
            (format "Process %s: %s"
                    (process-name proc)
                    (string-trim event))))))
      (remote-runner-log "Command launched successfully"))))

(defun remote-runner-update-config (new-config)
  "Update remote-runner-config with NEW-CONFIG."
  (setq remote-runner-config
        (append
         ;; Remove any existing keys that are in new-config
         (cl-remove-if (lambda (pair)
                         (assq (car pair) new-config))
                       remote-runner-config)
         new-config)))

(defun remote-runner-set-args (args)
  "Set the Python arguments in the configuration."
  (interactive "sEnter arguments: ")
  (setf (alist-get 'python-args remote-runner-config) args)
  (message "Arguments set to: %s" args))

(defun remote-runner-execute-current-buffer (&optional arg)
  "Execute current buffer on remote host using stored arguments.
With universal argument ARG, show the transient interface for argument input."
  (interactive "P")
  (if arg
      ;; With prefix arg, use transient for arguments
      (remote-runner-execute-current-buffer-with-transient)
    ;; Otherwise use stored args
    (let* ((local-file (buffer-file-name))
           (config remote-runner-config)
           (host (alist-get 'host config))
           (args (alist-get 'python-args config)))

      ;; Basic validation
      (when local-file
        (unless (remote-runner-test-connection host)
          (error "Cannot connect to host %s" host))

        ;; Execute with current config args
        (remote-runner-log (format "Starting execution with args: %s" args))
        (remote-runner-copy-current-buffer)
        (remote-runner-execute-with-args args)))))

;; Transient integration (loaded conditionally)
(when (featurep 'transient)
  (require 'transient)

  (defun remote-runner-verify-transient-setup ()
    "Verify transient package is properly setup for argument handling."
    (and (featurep 'transient)
         (fboundp 'transient-define-prefix)))

  (defun remote-runner-execute-current-buffer-with-transient ()
    "Execute current buffer with arguments from transient interface."
    (interactive)
    (let* ((local-file (buffer-file-name))
           (config remote-runner-config)
           (host (alist-get 'host config)))

      ;; Validation
      (unless local-file
        (error "No file associated with current buffer"))
      (unless (remote-runner-test-connection host)
        (error "Cannot connect to host %s" host))

      ;; Copy file first
      (remote-runner-copy-current-buffer)

      ;; Show transient menu - execution continues in transient--python-cli-done
      (transient-setup-python-args))))

;; ;; ;; Key bindings
;; (global-set-key (kbd "C-c r e") 'remote-runner-execute-current-buffer)
;; ;; (global-set-key (kbd "C-c r s") 'remote-runner-set-args)
;; ;; (global-set-key (kbd "C-c r l") 'remote-runner-show-log)
;; ;; (global-set-key (kbd "C-c r c") 'remote-runner-clear-log)

(provide 'remote-runner)
