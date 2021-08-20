(use-package emacs
  :custom
  (;; File locations
   (custom-file (concat user-emacs-directory
			dk/user-emacs-etcdir
			"custom.el"))
   (recentf-save-file (concat user-emacs-directory
			      dk/user-emacs-etcdir
			      "recentf"))
   (default-directory dk/user-system-base-path)
   (eshell-aliases-file (concat user-emacs-directory
				dk/user-emacs-etcdir
				"aliases"))
   (backup-directory-alist
    `((".*" . ,temporary-file-directory)))
   (auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))
   (save-place-file (concat user-emacs-directory
			    dk/user-emacs-etcdir
			    "places"))
   ;; Emacs Built-In Quality of life improvements
   (ring-bell-function 'ignore)
   (frame-title-format '("EMACS - " emacs-version))
   (display-time-24hr-format t)
   (display-time-day-and-date nil)
   ;; File specific variables
   (comint-prompt-read-only t)
   (load-prefer-newer t)
   (auto-revert-remote-files nil)
   (sentence-end-double-space nil)
   (require-final-newline t)
   (initial-scratch-message nil)
   (backup-by-copying t)
   (delete-old-versions t)
   (version-control t)
   ;; Movement preferences
   (scroll-margin 8)
   (indent-tabs-mode t))
  :init
  ;; Functions to enable certain emacs behaviours
  (cua-mode t)
  (global-auto-revert-mode t)
  (display-time-mode nil)
  (save-place-mode t)
  (global-hl-line-mode t)
  ;; Change the annoying yes or no to y or n
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Set default encoding system
  (set-language-environment "UTF-8"))

(provide 'emacs.el)
