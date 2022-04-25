(use-package no-littering
  :custom
  ((no-littering-etc-directory
    (concat user-emacs-directory
	    dk/user-emacs-etcdir))
   (no-littering-var-directory
    (concat user-emacs-directory
	    dk/user-emacs-etcdir))
   ;; ((auto-save-file-name-transforms
   ;;   '((".*" "~/.emacs.d/var/auto-save/" t)))
   ))

(use-package auto-package-update
  :defer t
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
   (lambda () (dk/log 'info "I will update packages now"))))

(use-package good-scroll
  :defer t
  :hook
  ((good-scroll-mode
    .
    (lambda ()
      (if (equal scroll-margin 0)
	  (setq scroll-margin 8)
	(setq scroll-margin 0)))))
  :config
  (defun dk/trackpad-mode ()
    "Alias for good-scroll-mode."
    (interactive)
    (good-scroll-mode t)))

(use-package dtrt-indent
  :config
  (dtrt-indent-global-mode t))

(use-package esup)

(use-package posframe)

(provide 'base-config)
