(defvar installer-info-list '(:name nil :keyboard nil :email nil))
(defconst config-file "~/.oec.el")

(defun get-installation-infos ()
  (message "Welcome to the interactive installer.")
  (message "The following information are not transmitted and")
  (message "not stored in the git directory.")
  (plist-put installer-info-list :name (read-from-minibuffer "Your Name: "))
  (plist-put installer-info-list :email (read-from-minibuffer "Your Email: "))
  (plist-put installer-info-list :keyboard
	     (y-or-n-p "Do you use a 40% Keyboard? ")))

(defun apply-installation-info ()
  (message "Installing user config file here: %s" config-file)
  (copy-file (concat user-emacs-directory "setup/template.el")
	     config-file)
  (with-temp-buffer
    (let ((inhibit-message t))
      (find-file config-file)
      (replace-string "{NAME}" (plist-get installer-info-list :name))
      (goto-char 1)
      (replace-string "{KEYBOARD}" (format "%s" (plist-get installer-info-list
							   :keyboard)))
      (goto-char 1)
      (replace-string "{EMAIL}" (plist-get installer-info-list :email))
      (goto-char 1)
      (save-buffer))))

(defun check-user-file-exists ()
  (if (not (file-exists-p config-file))
      (progn (get-installation-infos)
	     (apply-installation-info))
    (progn (message "The file already exists. If you start new,")
	   (message "please delete the following file: %s." config-file))))

(defun install ()
  (check-user-file-exists))

(defun uninstall ()
  (when (y-or-n-p (format "Do you want to delete your config (%s) file?"
			  config-file))
    (delete-file config-file)))
