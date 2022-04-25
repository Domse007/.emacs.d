(require 'base-module-declaration)

(defun dk/resolve-modules ()
  "Resolve the modules that are defined by the user."
  (let ((dependency-modules nil)
	(pre-to-be-loaded dk/to-be-loaded-modules))
    (setq dk/to-be-loaded-modules nil)
    ;; Loop through all user defined modules and gather deps.
    (dolist (user-mod dk/user-defined-modules)
      (dolist (config-mod dk/optional-modules)
	(let ((config-mod-name (plist-get config-mod :name)))
	  (when (eq user-mod config-mod-name)
	    (progn
	      (dolist (config-mod-s (plist-get config-mod :deps))
		(push config-mod-s dependency-modules))
	      (push config-mod-name dk/to-be-loaded-modules))))))
    ;; Check if deps are already defined.
    (dolist (dependency dependency-modules)
      (unless (member dependency dk/to-be-loaded-modules)
	(push dependency dk/to-be-loaded-modules)))
    ;; Reintroduce the base files. This is necessary otherwise the
    ;; order is not correct.
    (dolist (pre pre-to-be-loaded)
      (push pre dk/to-be-loaded-modules))))

(defun dk/load-modules ()
  "Load all modules that are defined by the user."
  (dolist (module dk/to-be-loaded-modules)
    (dk/log 'info "Loading " (symbol-name module) ".")
    (require module)))

(defun dk/run-hooks ()
  "Function that runs all hooks."
  (dk/log 'info "Running custom after init hooks.")
  (run-hooks 'dk/custom-after-init-hook))

(provide 'base-module-resolving)
