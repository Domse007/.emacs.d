(defconst dk/user-config-simple-file "~/.oec.el"
  "File location where basic user costomization is performed.")

(defconst dk/user-config-directory "~/.oec.d/"
  "Directory where the user customizations are located.")

(defconst dk/user-config-file "init.el"
  "File where the customization by the user is performed.")

(defun dk/user-config-get-user-file ()
  "Build the path to the user config file."
  (let ((simple-file dk/user-config-simple-file)
        (complex-file (concat dk/user-config-directory dk/user-config-file)))
    (if (file-exists-p simple-file)
        simple-file
      complex-file)))

(provide 'base-user-config)
