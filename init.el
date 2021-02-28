;; entry point of the emacs config.

;; set the correct encoding for all files:
(set-language-environment "UTF-8")

;; load custom variables
(setq dk/user-emacs-subdir "/modules")

(load-file (concat user-emacs-directory dk/user-emacs-subdir "/vars.el"))

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

;;(when (not (package-installed-p 'use-package))
;;  (package-refresh-contents)
;;  (package-install 'use-package))

(require 'use-package)
(use-package use-package
  :custom (use-package-compute-statistics t))

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

;; check if temp file exists
(defun dk/check-temp-dir-exists ()
  (if (equal (file-directory-p "~/.emacs.d/var/") nil)
      (progn (make-directory "~/.emacs.d/var/")
	     (message "Created var dir."))
    (message "Directory /var/lsp/ exists."))
  (if (equal (file-directory-p "~/.emacs.d/var/lsp/") nil)
      (progn (make-directory "~/.emacs.d/var/lsp/")
	     (message "Created /var/lsp/ dir."))
    (message "Directory /var/lsp/ exists.")))

(dk/check-temp-dir-exists)

;; check which theme should be loaded
(defvar dk/theme-light-choice nil)

(add-to-list 'command-switch-alist '("--light" . (lambda (args))))

(if (member "--light" command-line-args)
    (setq dk/theme-light-choice t))

;; load the custom config:
(load-file (concat user-emacs-directory dk/user-emacs-subdir "/emacs.el")) 
(load-file (concat user-emacs-directory dk/user-emacs-subdir "/qol.el")) 
(load-file (concat user-emacs-directory dk/user-emacs-subdir "/design.el")) 
(load-file (concat user-emacs-directory dk/user-emacs-subdir "/navigation.el")) 
(load-file (concat user-emacs-directory dk/user-emacs-subdir "/org-mode.el")) 
(load-file (concat user-emacs-directory dk/user-emacs-subdir "/programming.el"))
(load-file (concat user-emacs-directory dk/user-emacs-subdir "/custom-funcs.el"))
(load-file (concat user-emacs-directory dk/user-emacs-subdir "/test.el"))
(setq custom-file (concat user-emacs-directory "/var/custom.el"))

(setq dk/custom-org-std-user "Dominik Keller")

(message "Config initialisation ends here.")

(provide 'init.el)
