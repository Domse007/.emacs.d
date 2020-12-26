(use-package emacs 
	     :config
	     ;; all sorts of variables
	     (setq ring-bell-function 'ignore
		   frame-title-format '("EMACS - " emacs-version ": %b - %m" "-mode")
		   initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))
		   recentf-save-file (expand-file-name "recentf" "~/.emacs.d/recent")
		   default-directory user-system-base-path
		   display-time-24hr-format t
		   display-time-day-and-date t
		   comint-prompt-read-only t
		   load-prefer-newer t)
	     ;; enable additional information in modeline
	     (display-time-mode 1)
	     (display-battery-mode 1)
	     (save-place-mode 1)
	     ;; font
	     (set-face-attribute 'default nil
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
	     (set-face-background hl-line-face "#090405")
	     ;; gloabl line numbers
	     (global-linum-mode)
	     :hook (after-make-frame-functions . my/disable-scroll-bars))
