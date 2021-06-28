(defconst dk/windows-deps
  '(("git" . nil)
    ("sqlite3" . t)
    ("aspell" . t)
    ("aspell-de" . t)
    ("aspell-en" . t)
    ("rustup" . t)
    ("miktex" . t))
  "List of packages required on windows.")

(defconst dk/linux-deps
  '("sqlite3"
    "aspell"
    "aspell-de"
    "aspell-en"
    "rustup"
    "texlive")
  "List of packages required on windows.")

(defun dk/install-package (&rest args)
  (let ((package-manager (plist-get args :pm))
	(package-manager-args (plist-get args :arg))
	(package-manager-prefix (plist-get args :pre))
	(package-manager-prefix-required (plist-get args :req))
	(package-manager-package (plist-get args :pack))
	(package-manager-confirm (plist-get args :conf)))
    (shell-command
     (concat package-manager
	     " "
	     package-manager-args
	     " "
	     (when package-manager-prefix-required
	       package-manager-prefix)
	     package-manager-package
	     " "
	     package-manager-confirm))))

(defun test ()
  (interactive)
  (dk/test-install
   :pm "pacman"
   :arg "-S"
   :pre (if (equal system-type 'windows-nt)
	    "mingw-w64-x86_64-"
	  "")
   :req nil
   :pack "git"
   :conf "--noconfirm"))

;; This function is called, by `dk/check-config-files'
(defun dk/install-packages ()
  (interactive)
  (when (equal system-type 'windows-nt)
    (dolist (item dk/windows-deps)
      (dk/install-package
       :pm "pacman"
       :arg "-S"
       :pre "mingw-w64-x86_64-"
       :req (cdr item)
       :pack (car item)
       :conf "--noconfirm"))))

(provide 'custom-install.el)
