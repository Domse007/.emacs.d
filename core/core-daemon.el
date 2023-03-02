(require 'dashboard)

;; Fix some stuff that is not available when the daemon starts.
;;------------------------------------------------------------------------------

(defun dk/setup-daemon-visuals ()
  "Let emacs behave like emacs was started without the daemon."
  (when (display-graphic-p)
    (with-selected-frame (selected-frame)
      (setq dk/default-font (dk/select-default-font))
      (dk/set-default-font)))
  (when (and (get-buffer dashboard-buffer-name)
             (>= (length command-line-args) 1))
    (dk/log 'info "Switching to buffer.")
    (switch-to-buffer dashboard-buffer-name)))

(unless (display-graphic-p)
  (add-hook 'server-after-make-frame-hook #'dk/setup-daemon-visuals))

;; Ask if the current frame should be killed.
;;------------------------------------------------------------------------------

(defun dk/kill-emacs-daemon-fix (orig-fun &rest args)
  "For some weird reason emacs in daemon mode does not ask if you reallly want
to kill the frame"
  (message "called.")
  (when (yes-or-no-p "Really exit Emacs?")
    (apply orig-fun args)))

(when (daemonp)
  (advice-add 'save-buffers-kill-terminal :around #'dk/kill-emacs-daemon-fix))

(provide 'core-daemon)
