(use-package no-littering
  :ensure t
  :custom ((no-littering-etc-directory
	    (expand-file-name "A_temp/config/" user-emacs-directory))
	   (no-littering-var-directory
	    (expand-file-name "A_temp/data/" user-emacs-directory))))

(use-package restart-emacs
  :ensure t
  :config (setq restart-emacs-restore-frames t))

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

(use-package unkillable-scratch
  :ensure t
  :config (unkillable-scratch t))

(use-package memory-usage
  :ensure t)
