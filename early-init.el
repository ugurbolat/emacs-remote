
(setq default-frame-alist '((background-color . "#000000")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))


;;; early-init.el --- Doom's universal bootstrapper -*- lexical-binding: t -*-

;; stealing doom's early-init and doom-start as it is so good!

;;(message "early hello begin")

;; PERF: Garbage collection is a big contributor to startup times. This fends it
;;   off, but will be reset later by `gcmh-mode' (or in doom-cli.el, if in a
;;   noninteractive session). Not resetting it later causes stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; PERF: Don't use precious startup time checking mtime on elisp bytecode.
;;   Ensuring correctness is 'doom sync's job, not the interactive session's.
;;   Still, stale byte-code will cause *heavy* losses in startup efficiency, but
;;   performance is unimportant when Emacs is in an error state.
(setq load-prefer-newer noninteractive)

;; UX: Respect DEBUG envvar as an alternative to --debug-init, and to make
;;   startup sufficiently verbose from this point on.
(when (getenv-internal "DEBUG")
  (setq init-file-debug t
        debug-on-error t))


(setq doom-start-file (expand-file-name "src/doom/doom-start" user-emacs-directory))

(load doom-start-file)
;;(require 'doom-start)



;;(message "early hello end")

;;; early-init.el ends here
