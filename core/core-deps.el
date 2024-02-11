(require 'cl-macs)
(require 'dash)

(defvar core-deps-dependencies nil
  "List of all external dependencies.")

(cl-defstruct dependency
  "Describes an external dependency."
  (program :type 'string)
  (instructions :type 'string)
  (check t :type 'boolean))

(defun new-external-dependency! (program &optional nocheck)
  "Add a new external program to `dk/external-dependencies'. It is either a
symbol or a cons. If it is a symbol, it is just the name of the dependency. If
it is a cons cell, the car is the same symbol, but the cdr is a string with
installation instructions."
  (let* ((name (if (consp program)
		   (symbol-name (car program)) (symbol-name program)))
	 (instr (when (consp program) (cdr program)))
	 (dep (make-dependency :program name
			       :instructions instr
			       :check (not nocheck))))
    (unless (-filter (lambda (elem)
		       (string-equal (dependency-program elem) name))
		     core-deps-dependencies)
      (push dep core-deps-dependencies))))

(defun dk/check-external-deps ()
  "Check if external programs are available."
  (interactive)
  (let ((missing-alist nil))
    (dolist (program core-deps-dependencies)
      (when (dependency-check program)
	(dk/log 'info "Checking %s..." (dependency-program program))
	(unless (executable-find (dependency-program program))
	  (push missing-alist (dependency-program program)))))
    (if (not missing-alist)
	(dk/log 'info "No missing dependencies.")
      (dk/log 'error "Missing following dependencies: %s"
	      (substring (format "%s" missing-alist) 1 -1)))))

(defun dk/describe-external-dependency ()
  "Report the installation process for an external dependency."
  (interactive)
  (let* ((name-list (mapcar (lambda (thing) (dependency-program thing))
                            core-deps-dependencies))
         (user-input (completing-read "Dependency: " name-list nil t)))
    (dolist (dep core-deps-dependencies)
      (if (string= (dependency-program dep) user-input)
	  (if (dependency-instructions dep)
	      (message (message "%s can be installed with the following command: %s"
				user-input (dependency-instructions dep)))
	    (message "No instructions are available."))))))

(provide 'core-deps)
