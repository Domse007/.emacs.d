;;; qol.el - This is a configuration to porvide quality of life features.

;; commentary:
;; - The code doesen't depend on any additional binaries.

;; Package that cleans the conifg directory.
(use-package no-littering
  :ensure t
  :custom ((no-littering-etc-directory
	    (expand-file-name "var/config/" user-emacs-directory))
	   (no-littering-var-directory
	    (expand-file-name "var/data/" user-emacs-directory))))

;; Package that restarts Emacs without killing buffers.
(use-package restart-emacs
  :ensure t
  :config (setq restart-emacs-restore-frames t))

;; Package that automatically updates packages if updates are available.
(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

;; Packages that makes the scratch buffer unkillable.
(use-package unkillable-scratch
  :ensure t
  :config (unkillable-scratch t))

;; Package that monitors memors usage.
(use-package memory-usage
  :ensure t)

;; Package to provide a modern list library.
(use-package dash
  :ensure t)

(provide 'qol.el)
;;; qol.el ends here
