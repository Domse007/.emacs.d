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

(provide 'use-package.el)
