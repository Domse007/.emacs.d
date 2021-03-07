;;; emacs.el - This is a configuration to make adjustments to emacs that aren't packages.

;; commentary:
;; - The code doesen't depend on any additional binaries.

;; configure base emacs
(use-package emacs 
	     :config
	     ;; all sorts of variables
	     (setq ring-bell-function 'ignore
		   frame-title-format '("EMACS - " emacs-version ": %b - %m" "-mode")
		   initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))
		   recentf-save-file (expand-file-name "recentf" "~/.emacs.d/var/recent")
		   default-directory user-system-base-path
		   ;; display-time-24hr-format t
		   ;; display-time-day-and-date t
		   comint-prompt-read-only t
		   load-prefer-newer t
		   scroll-conservatively 100
		   display-time-24hr-format t
		   ;; Don't assume that sentences should have two spaces after
		   ;; periods. This ain't a typewriter.
		   sentence-end-double-space nil
		   require-final-newline t
		   confirm-kill-emacs 'y-or-n-p
		   initial-scratch-message nil
		   delete-by-moving-to-trash t
		   indent-tabs-mode t
		   indent-line-function 'insert-tab
		   eshell-aliases-file "~/.emacs.d/var/eshell/aliases"
		   byte-compile-warnings '(cl-functions))
	     (setq auto-save-file-name-transforms
		   `((".*" "~/.emacs.d/var/auto-save" t)))
	     ;; enable additional information in modeline
	     (display-time-mode 1)
	     (display-battery-mode 1)
	     (save-place-mode 1)
	     ;; When something changes a file, automatically refresh the buffer
	     ;; containing that file so they can't get out of sync.
	     (global-auto-revert-mode t)
	     ;; font
	     (set-face-attribute 'default nil
				 ;;:family "Consolas")
				 :family "Hack")
	     (defalias 'yes-or-no-p 'y-or-n-p)
	     ;; disable scroll bars in daemon mode
	     (defun my/disable-scroll-bars (frame)
	       (modify-frame-parameters frame
					'((vertical-scroll-bars . nil)
					  (horizontal-scroll-bars . nil))))
	     ;; close dired buffers after visiting them
	     (eval-after-load "dired"
	       (lambda ()
		 (put 'dired-find-alternate-file 'disabled nil)
		 (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file)))
	     ;; highlight current line
	     (global-hl-line-mode)
	     
	     (when (equal dk/theme-light-choice nil)
	       (set-face-background hl-line-face "#090405"))

	     (server-start))

(provide 'emacs.el)
;; emacs.el ends here
