(defvar dk/declared-modules nil
  "List of plist that contains all modules.")

(defvar dk/to-be-loaded-modules nil
  "List of names of modules that will be `required'.")

(defvar dk/optional-modules nil
  "List of plists with declarations of optional modules.")

(defvar dk/custom-after-init-hook nil
  "List of lambdas that will be called after config loading.")

(defmacro dk/declare-module! (name docs &rest args)
  "Default way of declaring modules."
  `(let* ((arg-list (list ,@args))
	  (module-name ,name)
	  (module-docs ,docs)
	  (module-dependencies (plist-get arg-list :depends-on))
	  (module-after-init (plist-get arg-list :after-init))
	  (module-load-p (plist-get arg-list :load-default))
	  (module-dir (plist-get arg-list :dir))
	  (module-optional (plist-get arg-list :optional)))
     (cond (module-load-p
	    (progn (push module-name dk/to-be-loaded-modules)
		   (unless (eq module-after-init nil)
		     (push module-after-init dk/custom-after-init-hook))
		   (unless (eq module-dependencies nil)
		     (dolist (dep module-dependencies)
		       (push dep dk/to-be-loaded-modules)))))
	   (module-optional
	    (progn (push `(:name ,module-name :deps ,module-dependencies)
			 dk/optional-modules))))
     (push `(:name ,module-name :description ,module-docs
		   :deps ,module-dependencies :load-default ,module-load-p
		   :dir ,module-dir :optional ,module-optional)
	   dk/declared-modules)))

(add-to-list 'load-path dk/config-core-path)
(add-to-list 'load-path dk/config-optional-path)

(dk/declare-module!
 'early-init "The early-init file."
 :load-default nil
 :dir user-emacs-directory)

(dk/declare-module!
 'init "The main init file."
 :load-default nil
 :dir user-emacs-directory)

(dk/declare-module!
 'base-use-package "Setup of use-package"
 :load-default t
 :optional nil
 :dir dk/config-core-path)

(dk/declare-module!
 'base-config "Setup of invisible packages"
 :load-default t
 :optional nil
 :dir dk/config-core-path)

(dk/declare-module!
 'base-emacs "Setup of built-in things."
 :load-default t
 :optional nil
 :dir dk/config-core-path)

(dk/declare-module!
 'base-design "Definitions of visible related packages."
 :load-default t
 :optional nil
 :dir dk/config-core-path)

(dk/declare-module!
 'base-evil "Setup evil key bindings."
 :load-default nil
 :optional t
 :dir dk/config-core-path)

(dk/declare-module!
 'custom-search "Custom function for querying config."
 :load-default nil
 :optional t
 :dir dk/config-optional-path)

(dk/declare-module!
 'custom-funcs "General custom funcs."
 :load-default nil
 :optional t
 :dir dk/config-optional-path)

(dk/declare-module!
 'custom-theme "Definitions of theme related packages."
 :load-default nil
 :optional t
 :dir dk/config-optional-path)

(dk/declare-module!
 'custom-helm "Setup of helm packages."
 :load-default nil
 :optional t
 :dir dk/config-optional-path
 :after-init (lambda () (helm-posframe-enable)))

(dk/declare-module!
 'custom-ivy "Setup of ivy packages."
 :load-default nil
 :optional t
 :dir dk/config-optional-path)

(dk/declare-module!
 'text-org-mode "Setup of org-mode."
 :load-default nil
 :optional t
 :dir dk/config-optional-path)

(dk/declare-module!
 'text-org-spell "Setup of spell checking."
 :load-default nil
 :optional t
 :dir dk/config-optional-path)

(dk/declare-module!
 'text-org-roam "Setup of org-roams."
 :load-default nil
 :optional t
 :dir dk/config-optional-path)

(dk/declare-module!
 'programming-base "Setup of basic programming features."
 :load-default nil
 :optional t
 :dir dk/config-optional-path)

(dk/declare-module!
 'programming-rust "Setup rust development environment."
 :load-default nil
 :optional t
 :depends-on '(programming-base)
 :dir dk/config-optional-path)

(dk/declare-module!
 'programming-elisp "Setup elisp development environment."
 :load-default nil
 :optional t
 :depends-on '(programming-base)
 :dir dk/config-optional-path)

(dk/declare-module!
 'programming-python "Setup python development environment."
 :load-default nil
 :optional t
 :depends-on '(programming-base)
 :dir dk/config-optional-path)

(dk/declare-module!
 'programming-haskell "Setup haskell development environment."
 :load-default nil
 :optional t
 :depends-on '(programming-base)
 :dir dk/config-optional-path)

(provide 'base-module-declaration)
