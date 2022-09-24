(use-package emacs
  :custom
  (;; File locations
   (custom-file (concat dk/user-emacs-cache-dir "custom.el"))
   (recentf-save-file (concat dk/user-emacs-cache-dir "recentf"))
   (default-directory dk/user-system-base-path)
   (eshell-aliases-file (concat dk/user-emacs-cache-dir "aliases"))
   (backup-directory-alist `((".*" . ,temporary-file-directory)))
   (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
   (save-place-file (concat dk/user-emacs-cache-dir "places"))
   (nsm-settings-file (concat dk/user-emacs-cache-dir "network-security.data"))
   ;; Emacs Built-In Quality of life improvements
   (ring-bell-function 'ignore)
   (frame-title-format '("EMACS - " emacs-version))
   (display-time-24hr-format t)
   (display-time-day-and-date nil)
   (display-time-default-load-average nil)
   (column-number-mode t)
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
   (scroll-step 1)
   (scroll-conservatively 101)
   ;; (indent-tabs-mode t)
   (confirm-kill-emacs 'y-or-n-p)
   ;;
   (mouse-yank-at-point t)
   ;; disable cl-warnings at startup
   (byte-compile-warnings '(cl-functions))
   (linum-format "%5d")
   (inhibit-startup-message t)
   ;; initial major mode
   (initial-major-mode 'emacs-lisp-mode))
  :init
  ;; Functions to enable certain emacs behaviours
  (cua-mode t)
  (global-auto-revert-mode t)
  (display-time-mode nil)

  ;; Enable battery usage. Disabled if not available.
  (unless (string-match-p "N/A" (battery))
    (display-battery-mode 1)) 
  
  (save-place-mode t)
  (global-hl-line-mode t)
  ;; Change the annoying yes or no to y or n
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Set default encoding system
  (set-language-environment "UTF-8")

  (global-unset-key (kbd "<menu>"))
  ;; (global-unset-key (kbd "<mouse-1>"))
  ;; (global-unset-key (kbd "<down-mouse-1>"))
  (global-unset-key (kbd "<insert>"))
  
  (if (boundp 'pixel-scroll-precision-mode)
      (pixel-scroll-precision-mode t)
    (dk/log 'warning "This emacs installation does not support "
	    "pixel-scroll-precision-mode."))

  (if (member dk/default-font (font-family-list))
      (progn (dk/log 'info "Setting font: " dk/default-font)
	     (set-face-attribute 'default nil :font dk/default-font))
    (dk/log 'error dk/default-font " is not available. Maybe install it."))

  (defcustom dk/default-coding-system 'utf-8-unix
    "Default coding system that is used. This must be set."
    :type 'symbol
    :group 'dk/config)

  (defun dk/set-default-coding-system ()
    "Set the default coding system."
    (interactive)
    (if (not (eq dk/default-coding-system nil))
	(progn (dk/log 'info "Setting default coding system: "
		       (symbol-name dk/default-coding-system))
	       (prefer-coding-system 'utf-8-unix))
      (dk/log 'error "No default coding system defined.")))

  (dk/set-default-coding-system)
  :bind
  (("C-k" . kill-whole-line)
   ("M-p" . backward-paragraph)
   ("M-n" . forward-paragraph)
   ("C-r" . undo-redo))
  :hook
  ((prog-mode . electric-pair-mode)
   (prog-mode . display-line-numbers-mode)
   (prog-mode . subword-mode)
   (after-init-hook . auto-revert-mode)))

(provide 'base-emacs)
