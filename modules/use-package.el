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
   (use-package-compute-statistics t)
   (use-package-always-defer t)))

(use-package quelpa-use-package
  :custom
  ((quelpa-upgrade-interval 7)
   (quelpa-dir (concat user-emacs-directory
		       dk/user-emacs-etcdir
		       "quelpa/"))))

(defcustom dk/checked-features-loaded '()
  ""
  :type '(repeat symbol))

(defcustom dk/checked-features-not-loaded '()
  ""
  :type '(repeat symbol))

(defun dk/report-unloaded-features ()
  ""
  (interactive)
  (let ((res ""))
    (dolist (elem dk/checked-features-not-loaded)
      (setq res (concat res ", " elem)))
    res))

(defun dk/check-loadable (req-version vers)
  ""
  (interactive)
  (if (version<= emacs-version req-version)
      (progn (vers)
	     (add-to-list dk/checked-features-loaded (quote vers)))
    (add-to-list dk/checked-features-not-loaded (quote vers))))

(provide 'dk/use-package)
