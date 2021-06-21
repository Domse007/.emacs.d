(require 'package)

(setq package-archives '(("org" . "https://orgmode.org/elpa/")
			                   ("gnu" . "https://elpa.gnu.org/packages/")
			                   ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(while (not (package-installed-p 'use-package))
  (sleep-for 1))

(require 'use-package)

(use-package use-package
	     :custom
	     ((use-package-always-ensure t)
	      (use-package-compute-statistics t)))

(use-package quelpa-use-package
	     :ensure t)

(use-package auto-package-update
  :custom
  ((auto-package-update-interval 3)
   (auto-package-update-prompt-before-update t)
   (auto-package-update-delete-old-versions t)
   (auto-package-update-hide-results t)
   (auto-package-update-last-update-day-filename
    (concat dk/user-emacs-etcdir ".last-update-day")))
  :hook
  (auto-package-update-before-hook .
          (lambda () (message "I will update packages now"))))

(provide 'use-package.el)
