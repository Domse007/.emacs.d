(use-package no-littering
  :custom
  ((no-littering-etc-directory
    (concat user-emacs-directory
	    dk/user-emacs-etcdir
	    "etc/"))
   (no-littering-var-directory
    (concat user-emacs-directory
	    dk/user-emacs-etcdir
	    "var/"))))

(use-package auto-package-update
  :custom
  ((auto-package-update-interval 3)
   (auto-package-update-prompt-before-update t)
   (auto-package-update-delete-old-versions t)
   (auto-package-update-hide-results t)
   (auto-package-update-last-update-day-filename
    (concat dk/user-emacs-etcdir ".last-update-day")))
  :hook
  (auto-package-update-before-hook
   .
   (lambda () (message "I will update packages now"))))

(use-package good-scroll
  :hook
  ((good-scroll-mode
    .
    (lambda ()
      (if (equal scroll-margin 0)
	  (setq scroll-margin 8)
	(setq scroll-margin 0)))))
  :config
  (defun trackpad-mode ()
    "Alias for good-scroll-mode."
    (interactive)
    (good-scroll-mode t)))

(use-package unkillable-scratch
  :config
  (unkillable-scratch t))

(provide 'config.el)
