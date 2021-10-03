;; Decrease the garbage collector limit to 8mb after
;; startup to ensure that there are no lags, when the
;; garbage collector kicks in.
(setq gc-cons-threshold 800000)

(defun dk/greet ()
  "Print a nice greeting after startup."
  (concat "Welcome back! Running GNU/Emacs "
	  emacs-version
	  ". The config is on version "
	  dk/config-version
	  "."))

(setq dashboard-banner-logo-title (dk/greet))

(helm-posframe-enable)

(global-unset-key (kbd "<menu>"))

(provide 'custom-after-init.el)
