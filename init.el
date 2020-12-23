;; entry point of the emacs config.

;; set the correct encoding for all files:
(set-language-environment "UTF-8")

;; ensure that packages are installed correctly:
(require 'package)

(setq package-archives '(("org" . "https://orgmode.org/elpa/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package quelpa-use-package
  :ensure t)

(add-to-list 'load-path "~/.emacs.d/resources/")

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

;; special key-bindings:
(cua-mode t)

;; custom functions to set the base path according to the system.
(defvar user-system-name (system-name)
  :string)

(defvar user-system-base-path ""
  :string)

(defun user-change-base-path ()
  (when (string-equal user-system-name "DESKTOP-9R2BNNM")
    (setq user-system-base-path "c:/Users/Dominik/DomiCloud/")))

(user-change-base-path)

;; load the custom config:
(load-file (concat user-emacs-directory "/config/emacs.el"))
(load-file (concat user-emacs-directory "/config/design.el"))
(load-file (concat user-emacs-directory "/config/navigation.el"))
(load-file (concat user-emacs-directory "/config/org-mode.el"))
(load-file (concat user-emacs-directory "/config/programming.el"))
(load-file (concat user-emacs-directory "/config/custom-funcs.el"))
(load-file (concat user-emacs-directory "/config/test.el"))
(setq custom-file (concat user-emacs-directory "/config/custom.el"))
