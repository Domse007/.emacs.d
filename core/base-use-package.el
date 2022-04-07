(require 'package)

(when (equal system-type 'windows-nt)
  (setq package-check-signature nil))

(when (version< "28.0.50" emacs-version)
  (setq package-native-compile t
	warning-minimum-level :emergency))

(setq package-archives
      '(("nongnu" . "http://elpa.nongnu.org/nongnu/")
	("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))

;; (setq package-user-dir (concat user-emacs-directory
;; 			       dk/user-emacs-etcdir
;; 			       "elpa/"))

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
  :custom
  ((quelpa-dir (concat user-emacs-directory
		       dk/user-emacs-etcdir
		       "quelpa/"))
   (quelpa-checkout-melpa-p nil)))

;; (setq use-package-always-defer t)

(provide 'base-use-package)
