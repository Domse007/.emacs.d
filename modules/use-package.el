(require 'package)

(when (equal system-type 'windows-nt)
  (setq package-check-signature nil))

(setq package-archives '(;("org" . "https://orgmode.org/elpa/")
			 ;("gnu" . "https://elpa.gnu.org/packages/")
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

;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; (straight-use-package 'use-package)

;; (setq straight-use-package-by-default t)

(provide 'use-package.el)


