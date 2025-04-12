;;; main.el --- User's Emacs Configuration -*- lexical-binding: t; -*-

;;; Paths and Directories
(load-file (expand-file-name "src/paths.el" user-emacs-directory))
(setq ub/emacs-configs-dir "~/emacs-configs")
(defvar elpaca-directory (expand-file-name (concat "elpaca-emacs-remote_" emacs-version) ub/emacs-configs-dir))
(unless (file-exists-p elpaca-directory)
  (make-directory elpaca-directory t))

;;; Autoloads fns
(load-file (expand-file-name "src/autoload.el" user-emacs-directory))

;;; Package Manager
(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                       :ref nil :depth 1 :inherit ignore
                       :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                       :build (:not elpaca--activate-package)))

(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (progn
          (apply #'call-process `(,"git" nil "*elpaca-bootstrap*" t "clone"
                                  ,@(when-let ((depth (plist-get order :depth)))
                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                  ,(plist-get order :repo) ,repo))
          (call-process "git" nil "*elpaca-bootstrap*" t "checkout"
                        (or (plist-get order :ref) "--"))
          (call-process (concat invocation-directory invocation-name) nil nil nil
                        "-Q" "-L" "." "--batch"
                        "--eval" "(byte-recompile-directory \".\" 0 'force)")
          (require 'elpaca)
          (elpaca-generate-autoloads "elpaca" repo))
      (error (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))

(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;; Use-package support
(elpaca elpaca-use-package
        (elpaca-use-package-mode))
(elpaca-wait)

;;; Defaults
(setq inhibit-startup-screen t
      initial-scratch-message ""
      make-backup-files nil
      create-lockfiles nil
      delete-by-moving-to-trash t
      enable-local-variables t
      cursor-type 'box
      scroll-step 1
      scroll-conservatively 10000
      truncate-partial-width-windows nil
      sentence-end-double-space nil
      require-final-newline t)

(setq-default indent-tabs-mode nil
              tab-width 4
              tab-always-indent nil
              fill-column 80
              word-wrap t
              truncate-lines t)

;;; UI Tweaks
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode 1)
(set-face-attribute 'default (selected-frame) :height 150)
(load-theme 'modus-vivendi-tritanopia t)
;; (load-theme 'modus-vivendi t)

(use-package spacious-padding
  :ensure (:fetcher github :repo "protesilaos/spacious-padding")
  :defer 0.1
  :custom
  ;; new
  (spacious-padding-widths
   '( :internal-border-width 15
      :header-line-width 4
      :mode-line-width 4
      :tab-width 4
      :right-divider-width 30
      :scroll-bar-width 8
      :fringe-width 8))
  :config
  (spacious-padding-mode 1))




;;; modeline
(use-package doom-modeline
  :ensure (:fetcher github :repo "seagle0128/doom-modeline")
  ;;:hook (after-init . doom-modeline-mode)
  :init (doom-modeline-mode 1) ;;NOTE above doesn't work
  :custom
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-major-mode-icon nil)
  :config
  (minions-mode 1)
  (defun doom-modeline-conditional-buffer-encoding ()
    "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
    (setq-local doom-modeline-buffer-encoding
                (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                   '(coding-category-undecided coding-category-utf-8))
                             (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                  t)))
  (add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

  ;; BUG emacs 30.1 show Python unexpected (instead of version)...
  (setq doom-modeline-env-enable-python nil)
  ;;(setq doom-modeline-env-version-length 10) 
  )

(use-package minions
  :ensure (:fetcher github :repo "tarsius/minions")
  :defer 0.1
  :after (doom-modeline)
  :hook (doom-modeline-mode . minions-mode))



;; ;; Enable built-in modules for time and battery
;; (display-time-mode 1)
;; (display-battery-mode 1)
;; Customize mode-line-format
;; (setq-default
;;  mode-line-format
;;  '(
;;    ;; Buffer info: name and modified status
;;    (:propertize "%b" face mode-line-buffer-id)
;;    " "
;;    (:eval (if (buffer-modified-p) "*" " "))
;;    "  "
;;    ;; Position: line and column
;;    "(%l:%c)  "
;;    ;; Major mode and minor modes
;;    mode-name
;;    "  "
;;    minor-mode-alist
;;    ;; Right-aligned elements
;;    (:eval (propertize " " 'display '((space :align-to (- right 40)))))
;;    "  "
;;    ;; Version control (git)
;;    vc-mode
;;    ;; Time and battery
;;    display-time-string
;;    "  "
;;    battery-mode-line-string
;;    ))


;;; Editing Convenience
(delete-selection-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

;;; Auto-Revert
(require 'autorevert)
(global-auto-revert-mode 1)
(setq auto-revert-use-notify t)
(add-hook 'dired-mode-hook 'auto-revert-mode)

;;; Hooks
(add-hook 'text-mode-hook #'visual-line-mode)

;;; Recent Files
;;(recentf-mode 1)
(use-package recentf
  :ensure nil
  ;;:defer 0.1
  :custom
  (recentf-auto-cleanup 30)
  (recentf-max-saved-items 200)
  :config
  (recentf-mode)
  (run-with-idle-timer 30 t 'recentf-save-list))


;;; fixing anoyying custom.el
(use-package cus-edit
  :defer t
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory) "Store customizations in a temp file")
  :config
  (load custom-file 'noerror)
  ;;(custom-file (make-temp-file "emacs-custom") "Store customizations in a temp file")
  ;;(custom-file null-device "Don't store customizations")
  )

;; ;;; setting for undo/redo
;; (use-package undo
;;   :ensure nil ;; built in feature, no need to ensure
;;   :bind
;;   ("C-z" . undo-only)
;;   ("C-S-z" . undo-redo))

;;; Window movements
(use-package window
  :ensure nil ;; built in feature, don't need to ensure
  :bind
  (("S-C-<left>" . shrink-window-horizontally)
   ("S-C-<right>" . enlarge-window-horizontally)
   ("S-C-<down>" . shrink-window)
   ("S-C-<up>" . enlarge-window)
   ("C-=" . text-scale-increase)
   ("C--" . text-scale-decrease)))

(use-package ace-window
  :ensure (:fetcher github :repo "abo-abo/ace-window")
  :defer t
  :bind
  ("C-x o" . ace-window))


;;; Helpful things
(use-package which-key
  :ensure (:fetcher github :repo "justbur/emacs-which-key")
  :defer 0.1
  :config
  (which-key-mode))

(use-package helpful
  :ensure (:fetcher github :repo "Wilfred/helpful")
  :defer t
  :bind
  ;; this looks weird w/ [...]
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  :init
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function))

;;;
(use-package vterm
  :ensure (:fetcher github :repo "akermu/emacs-libvterm")
  :defer 0.1
  :config
  (setq vterm-max-scrollback 10000)
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil))))

;;; Magit
(use-package transient
  :ensure (:fetcher github :repo "magit/transient")
  :defer t)
(use-package magit
  :ensure (:fetcher github :repo "magit/magit")
  :defer t
  ;; :bind
  ;; ("C-x g" . magit-status)
  )

;;; Dired
(use-package dired
  :ensure nil
  ;; NOTE don't work w/ other keybinding in dired-hacks
  ;;:bind
  ;;("C-c o -" . dired-jump)
  :config
  (global-set-key (kbd "C-c o -") 'dired-jump)
  (setq delete-by-moving-to-trash 't)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(use-package nerd-icons-dired
  :ensure (:fetcher github :repo "rainstormstudio/nerd-icons-dired")
  :config
  :hook (dired-mode . (lambda ()
                        (interactive)
                        (unless (file-remote-p default-directory)
                          (nerd-icons-dired-mode)))))

(use-package dired-hacks
  :ensure (:fetcher github :repo "Fuco1/dired-hacks")
  :defer t
  :after dired
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("<C-tab>" . dired-subtree-cycle)
        ("<backtab>" . dired-subtree-remove)
        ("e" . ub/ediff-files)
        ("<M-right>" . dired-find-file)
        ("<M-left>" . dired-up-directory))
  :config
  ;; dired extra
  (require 'dired-x)
  ;; TODO write the
  ;; subtree
  (require 'dired-subtree)
  ;;(setq dired-subtree-use-backgrounds nil)

  ;; TODO move this to autoload?
  (defun ub/ediff-files ()
    (interactive)
    (let ((files (dired-get-marked-files))
          (wnd (current-window-configuration)))
      (if (<= (length files) 2)
          (let ((file1 (car files))
                (file2 (if (cdr files)
                           (cadr files)
                         (read-file-name
                          "file: "
                          (dired-dwim-target-directory)))))
            (if (file-newer-than-file-p file1 file2)
                (ediff-files file2 file1)
              (ediff-files file1 file2))
            (add-hook 'ediff-after-quit-hook-internal
                      (lambda ()
                        (setq ediff-after-quit-hook-internal nil)
                        (set-window-configuration wnd))))
        (error "no more than 2 files should be marked"))))
  )


(use-package dired-hide-dotfiles
  :ensure t
  :bind
  (:map dired-mode-map
        ("." . dired-hide-dotfiles-mode))
  :hook
  (dired-mode . dired-hide-dotfiles-mode))


(use-package dired-rsync
  :ensure (:fetcher github :repo "stsquad/dired-rsync")
  :after dired
  :bind
  (:map dired-mode-map
        ("r" . dired-rsync))
  :config
  ;;(setq dired-rsync-command "rsync")
  (setq dired-rsync-options "-azh --progress")
  (setq dired-rsync-unmark-on-completion t)
  ;;(add-to-list 'mode-line-misc-info '(:eval dired-rsync-modeline-status 'append)
  )

(use-package dired-rsync-transient
  :ensure (:fetcher github :repo "stsquad/dired-rsync")
  ;;:after dired-rsync
  :bind (:map dired-mode-map
              ("C-c C-x" . dired-rsync-transient)))


;;; Remote Editing
(use-package tramp
  :ensure nil
  :config
  ;; REF: https://chatgpt.com/c/67f91cd8-fe60-800c-a4d3-0c4bb48fc3c7
  ;; Disable Version Control Integration on Remotes:
  ;; Emacs’ VC mode and related features (like showing Git status in the modeline)
  ;; can hammer TRAMP with extra operations
  (setq vc-ignore-dir-regexp 
        (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))

  ;; 
  ;;(setq tramp-auto-save-directory "/home/bolatu/emacs-configs/emacs-remote/.local/cache/tramp-autosave/")
  (setq tramp-auto-save-directory "/home/bolatu/emacs-configs/emacs-remote/tramp-autosave/")
  
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))


(use-package emacs
  ;;:ensure nil
  :custom
  (shell-file-name "/usr/bin/bash")
  (shell-command-switch "-c"))

(setq comint-buffer-maximum-size 2048)
(setq compilation-filter-hook '(comint-truncate-buffer ansi-color-compilation-filter))

;;;
(use-package quickrun
  :ensure (:fetcher github :repo "emacsorphanage/quickrun")
  :defer t
  :custom
  (quickrun-focus-p nil)
  (quickrun-timeout-seconds nil)
  :bind
  ("C-c r r" . quickrun)
  :config
  (quickrun-add-command "python"
    '((:command . "python3")
      (:exec . "%c %s")
      (:tempfile . nil)))
  ;;(remove-hook 'quickrun-after-run-hook '+eval-quickrun-scroll-to-bof-h)
  ;;(add-hook 'quickrun-after-run-hook 'ansi-color-compilation-filter)
  )

;; BUG: quickrun doesn't colorized due to the read-only-mode changes?
;; when I manually disable read-only-mode and run (ansi-color-apply-on-region (point-min) (point-max))
;; it does work!
;; (with-eval-after-load 'quickrun
;;   (defun quickrun--default-filter (proc output)
;;     "Custom override for quickrun filter to work with Emacs 30.1."
;;     (with-current-buffer (process-buffer proc)
;;       (let ((inhibit-read-only t)) ;; allow writing even in read-only buffers
;;         (goto-char (point-max))
;;         (let ((start (point)))
;;           (insert output)
;;           (ansi-color-apply-on-region start (point)))))))


;;;
(use-package realgud
  :ensure (:fetcher github :repo "realgud/realgud")
  :defer 0.1
  ;; when in python-mode set key C-c d d to realgud:pdb
  :bind
  (:map python-mode-map
        ("C-c d d" . realgud:pdb)
        )
  :custom
  (realgud-safe-mode nil))


;;;
(use-package csv-mode
  :ensure t
  :defer t
  :mode
  (("\\.[Cc][Ss][Vv]\\'" . csv-mode)))

(load-file (expand-file-name "src/completions.el" user-emacs-directory))

(load (expand-file-name "src/python.el" user-emacs-directory))

(load-file (expand-file-name "src/remote-runner-args-python.el" user-emacs-directory))
(when (featurep 'transient)
  (load-file (expand-file-name "src/remote-runner-args-python-transient.el" user-emacs-directory)))
(load-file (expand-file-name "src/remote-runner.el" user-emacs-directory))
