
;; Here is where the magic happens for our (Modern) Emacs.

;; MINIBUFFER
;; vertico, orderless, consult, marginalia, embark

(use-package vertico
  :ensure (:fetcher github :repo "minad/vertico")
  :defer 0.1
  :init
  (vertico-mode)
  :custom
  (vertico-count 15)
  (vertico-resize nil)
  (vertico-cycle t)
  :bind
  (:map vertico-map
        ("DEL" . vertico-directory-delete-char))
  :config
  (savehist-mode)
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;;(setq read-extended-command-predicate
  ;;      #'command-completion-default-include-p)
  (setq enable-recursive-minibuffers t))

(use-package orderless
  :ensure (:fetcher github :repo "oantolin/orderless")
  :defer 0.1
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure (:fetcher github :repo "minad/marginalia")
  :defer 0.1
  :config
  (marginalia-mode))

(use-package consult
  :ensure (:fetcher github :repo "minad/consult")
  :defer 0.1
  :after vertico
  :bind
  (("C-c f r" . consult-recent-file)
   ("C-x C-v" . consult-buffer)
   ("C-x C-'" . consult-ripgrep)
   ("C-S-s" . consult-line)
   )
  :custom
  (consult-line-start-from-top 't))

(use-package consult-dir
  :ensure (:fetcher github :repo "karthink/consult-dir")
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package embark
  :ensure t
  :defer 0.1
  :bind
  (("C-c a" . embark-act)         ;; pick some comfortable binding
   ;;("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :defer 0.1
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))



(use-package wgrep
  :ensure t
  :defer 0.1)


;; functions that use `consult-find` w/ default dirs
;; signature (defun consult-find (&optional dir initial)
(defun ub/consult-find-main (&optional initial)
  "Search for files with `find' in ~/main."
  (interactive "P")
  (consult-find "~/main" initial))
(defun ub/consult-find-home (&optional initial)
  "Search for files with `find' in ~/"
  (interactive "P")
  (consult-find "~/" initial))
(defun ub/consult-find-org (&optional initial)
  "Search for files with `find' in ~/main/org."
  (interactive "P")
  (consult-find "~/main/org" initial))
(defun ub/consult-find-zotero-pdf (&optional initial)
  "Search for files with `find' in ~/main/org."
  (interactive "P")
  (consult-find "~/main/library/Zotero-Library" initial))

;; same for `consult-ripgrep`
(defun ub/consult-ripgrep-main (&optional initial)
  "Search for files with `grep' in ~/main."
  (interactive "P")
  (consult-ripgrep "~/main" initial))
(defun ub/consult-ripgrep-home (&optional initial)
  "Search for files with `grep' in ~/"
  (interactive "P")
  (consult-ripgrep "~/" initial))
(defun ub/consult-ripgrep-org (&optional initial)
  "Search for files with `grep' in ~/main/org."
  (interactive "P")
  (consult-ripgrep "~/main/org" initial))
(defun ub/consult-ripgrep-dir (&optional initial)
  "Search for files with `grep' in current directory."
  (interactive "P")
  (consult-ripgrep "./" initial))

(defun ub/consult-ripgrep-buffer (&optional initial)
  "Search in current file using ripgrep."
  (interactive "P")
  (when-let ((file (buffer-file-name)))
    (let ((consult-ripgrep-args
           (format "rg --null --line-buffered --color=never --max-columns=500 --no-heading --line-number -g %s"
                   (file-name-nondirectory file))))
      (consult-ripgrep (file-name-directory file) initial))))



;; `consult-find` that asks the user dir which can be found interactively in minibuffer
(defun ub/consult-find-interactive ()
  "Search for files with `find' in a directory."
  (interactive)
  (consult-find (read-directory-name "Directory: ")))
;; same for `consult-ripgrep`
(defun ub/consult-ripgrep-interactive ()
  "Search for files with `grep' in a directory."
  (interactive)
  (consult-ripgrep (read-directory-name "Directory: ")))




;; IN BUFFER
;; corfu, orderless, cape, eglot, copilot


;; bind completion-at-point to C-;
(use-package emacs
  :ensure nil
  :bind
  ("C-;" . completion-at-point))

(use-package corfu
  :ensure (:fetcher github :repo "minad/corfu")
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode)
  :config
  ;;(corfu-echo-mode t) ;; either show up echo or popup
  (corfu-history-mode t)
  (corfu-popupinfo-mode t)

  ;; REF: ~/emacs-configs/doom-done-right/modules/completion/corfu/config.el
  (setq ;;corfu-auto t
        ;;corfu-auto-delay 0.24
        ;;corfu-auto-prefix 2
        global-corfu-modes
        '((not erc-mode
               circe-mode
               help-mode
               gud-mode
               vterm-mode)
          t)
        corfu-cycle t
        corfu-preselect 'prompt
        corfu-count 16
        corfu-max-width 120
        corfu-on-exact-match nil
        ;; corfu-quit-at-boundary (if (or (modulep! :completion vertico)
        ;;                                (modulep! +orderless))
        ;;                            'separator t)
        corfu-quit-no-match corfu-quit-at-boundary
        tab-always-indent 'complete)
  
  )


;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  ;; TODO
  (setq text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

;; corfu's eglot related configs
;; REF https://github.com/minad/corfu/wiki#configuring-corfu-for-eglot
;; Option 1: Specify explicitly to use Orderless for Eglot
(setq completion-category-overrides '((eglot (styles orderless))
                                      (eglot-capf (styles orderless))))
(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)


(use-package nerd-icons-corfu
  :ensure (:fetcher github :repo "LuigiPiucco/nerd-icons-corfu")
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


;; minad/cape
(use-package cape
  :ensure (:fetcher github :repo "minad/cape")
  :init
  (global-corfu-mode))
;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  :config
  ;; NOTE "This feature is EXPERIMENTAL and should only be used carefully in special scenarios."
  ;; Merge the dabbrev, dict and keyword capfs, display candidates together.
  (setq-local completion-at-point-functions
              (list (cape-capf-super #'cape-dabbrev #'cape-dict #'cape-keyword)))
  ;; Alternative: Define named Capf instead of using the anonymous Capf directly
  (defun cape-dabbrev-dict-keyword ()
    (cape-wrap-super #'cape-dabbrev #'cape-dict #'cape-keyword))
  (setq-local completion-at-point-functions (list #'cape-dabbrev-dict-keyword))
  )


;; note that eglot is built-in since emacs 29
;; eglot customizations
(use-package emacs
  :ensure nil
  :custom-face
  ;; NOTE setting font in early-init to avoid jitter/flashing
  ;;(default ((t (:height 160)))) ; set font size
  (eglot-highlight-symbol-face ((t (:foreground "#ffffff" :background "#10387c")))))

;; eglot's resized echo area display too annoying
;; REF https://joaotavora.github.io/eglot/#Eglot-Features
;; REF https://www.reddit.com/r/emacs/comments/16nnlwa/turn_of_eldoc_in_eglot_without_turning_of_symbol/
(use-package eglot
  :ensure nil
  :defer t
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-echo-area-prefer-doc-buffer t))



;; (use-package consult-eglot
;;   :ensure (:fetcher github :repo "mohkale/consult-eglot")
;;   :defer t
;;   :init
;;   (map! :after eglot
;;         :map eglot-mode-map
;;         [remap xref-find-apropos] #'consult-eglot-symbols))
(use-package consult-eglot
  :ensure (:fetcher github :repo "mohkale/consult-eglot")
  ;; :when (and (require 'eglot nil t)
  ;;            (require 'consult nil t))
  :bind (:map eglot-mode-map
              ([remap xref-find-apropos] . consult-eglot-symbols)))

(use-package flycheck-eglot
  :ensure (:fetcher github :repo "flycheck/flycheck-eglot")
  :hook (eglot-managed-mode . flycheck-eglot-mode))


(use-package consult-xref
  :after (consult xref)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))
