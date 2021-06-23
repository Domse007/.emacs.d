;; This function is called, by `dk/check-config-files'
(defun dk/install-packages ()
  "Iterate over `dk/system-packages' and check if the 
   executable exists and if it doesn't, install it."
  (let ((package-manager "")
	(package-manager-args ""))
    (setq package-manager
	  (read-string "Package manager: "))
    (setq package-manager-args
	  (read-string "Package manager install arg: "))
    (dolist (package dk/system-packages)
      (when (equal (executable-find package) nil)
	(shell-command
	 (concat package-manager
		 " "
		 package-manager-args
		 " "
		 package))))))

(provide 'custom-install.el)
