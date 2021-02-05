;; entry point of the emacs config.

;; set the correct encoding for all files:
(set-language-environment "UTF-8")

;; load custom variables
(load-file (concat user-emacs-directory "/config/vars.el"))

;; native compile, when available
;;(when (version< emacs-version "28.0.50")
;; (if (and (fboundp 'native-comp-available-p)
;; 	 (native-comp-available-p))
;;     (message "Native compilation is available")
;;   (message "Native complation is *not* available"))
;; (setq comp-deferred-compilation t)

;; ensure that packages are installed correctly:
(require 'package)

(setq package-archives '(("org" . "https://orgmode.org/elpa/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(setq package-check-signature nil)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package quelpa-use-package
  :ensure t)

(add-to-list 'load-path "~/.emacs.d/resources/")

;; special key-bindings:
(cua-mode t)

;; custom functions to set the base path according to the system.
(defun user-change-base-path ()
  (when (string-equal user-system-name "DESKTOP-9R2BNNM")
    (setq user-system-base-path "c:/Users/Dominik/DomiCloud/"))
  (when (string-equal user-system-name "SURFACEBOOK2")
    (setq user-system-base-path "c:/Users/Dominik/CloudStation/")))

(user-change-base-path)

;; load the custom config:
(load-file (concat user-emacs-directory "/config/emacs.el")) 
(load-file (concat user-emacs-directory "/config/qol.el")) 
(load-file (concat user-emacs-directory "/config/design.el")) 
(load-file (concat user-emacs-directory "/config/navigation.el")) 
(load-file (concat user-emacs-directory "/config/org-mode.el")) 
(load-file (concat user-emacs-directory "/config/programming.el"))
(load-file (concat user-emacs-directory "/config/custom-funcs.el"))
(load-file (concat user-emacs-directory "/config/test.el"))
(setq custom-file (concat user-emacs-directory "/config/custom.el"))

(setq dk/custom-org-std-user "Dominik Keller")

(message "Config initialisation ends here.")

(provide 'init.el)
