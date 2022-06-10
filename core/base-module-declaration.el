(defvar dk/declared-modules nil
  "List of plist that contains all modules.")

(defvar dk/to-be-loaded-modules nil
  "List of names of modules that will be `required'.")

(defvar dk/custom-after-init-hook nil
  "List of lambdas that will be called after config loading.")

(defmacro core-module! (name docs &rest args)
  "Default way of declaring modules."
  (declare (indent 1))
  `(let* ((arg-list (list ,@args))
	  (module-name ',name)
	  (module-docs ,docs)
	  (module-after-init (plist-get arg-list :after-init))
	  (module-load-p (plist-get arg-list :not-load))
	  (module-dir (plist-get arg-list :dir)))
     (progn (unless module-load-p
              (push module-name dk/to-be-loaded-modules))
	    (unless (eq module-after-init nil)
	      (push module-after-init dk/custom-after-init-hook)))
     (push `(:name ,module-name :description ,module-docs :dir ,module-dir)
	   dk/declared-modules)))

(defun dk/load-core ()
  "Load the core modules."
  (dolist (module (reverse dk/to-be-loaded-modules))
    (require module)))

(defmacro module! (name docs &rest args)
  "New way of declaring modules. This will be located in the according file."
  (declare (indent 1))
  `(let* ((arg-list ',args)
          (module-name ',name)
          (module-docs ,docs)
          (module-dependencies (plist-get arg-list :depends-on))
          (module-conflicts (plist-get arg-list :conflicts-with))
          (module-after-init (plist-get arg-list :after-init))
          (module-dir (plist-get arg-list :dir)))
     (push `(:name ,module-name :description ,module-docs :dir ,module-dir)
           dk/declared-modules)
     (if (listp module-conflicts)
         (dolist (conflicted module-conflicts)
           (when (featurep conflicted)
             (error "Module %s is incompatible with %s." module-name conflicted)))
       (when (featurep conflicts-with)
         (error "Module %s is incompatible with %s." module-name conflicted)))
     (dolist (dependency module-dependencies)
       (if (listp dependency)
           (let ((found nil))
             (dolist (dep dependency)
               (when (featurep dep)
                 (setq found t)))
             (when found
               (require (nth 1 dependency))))
         (require dependency)))))

(add-to-list 'load-path dk/config-core-path)
(add-to-list 'load-path dk/config-optional-path)

(core-module! early-init
  "The early-init file."
  :not-load t
  :dir user-emacs-directory)

(core-module! init
  "The main init file."
  :not-load t
  :dir user-emacs-directory)

(core-module! base-user-config
  "Definitions to be able to load the user config."
  :dir dk/config-core-path)

(core-module! base-version
  "Definitions of all version variables and functions"
  :dir dk/config-core-path)

(core-module! base-module-declaration
  "Definitions of all modules for this config."
  :dir dk/config-core-path)

(core-module! base-get-package
  "Module which allows to pull package the quelpa way without git."
  :dir dk/config-core-path)

(core-module! base-use-package
  "Setup of use-package"
  :dir dk/config-core-path)

(core-module! base-config
  "Setup of invisible packages"
  :dir dk/config-core-path)

(core-module! base-emacs
  "Setup of built-in things."
  :dir dk/config-core-path)

(core-module! base-design
  "Definitions of visible related packages."
  :dir dk/config-core-path)

(core-module! base-funcs
  "General custom funcs."
  :dir dk/config-core-path)

(core-module! base-user-config
  "Definitions for user file."
  :dir dk/config-core-path)

(provide 'base-module-declaration)
