(use-package lspce
  :quelpa (lspce :fetcher github :repo "zbelial/lspce" :files ("*"))
  :init
  (let ((default-directory (file-name-directory (locate-library "lspce"))))
    (message "Entering %s" (pwd))
    (unless (or (file-exists-p "lspce-module.so")
		(file-exists-p "lspce-module.d")
                (file-exists-p "lspce-module.dll"))
      (shell-command "cargo build --release")
      (cond ((eq system-type 'gnu/linux)
	     (progn (copy-file "target/release/liblspce_module.so"
			       "lspce-module.so" t)
		    (copy-file "target/release/liblspce_module.d"
			       "lspce-module.d" t)))
	    ((eq system-type 'windows-nt)
	     (progn (copy-file "target/release/lspce_module.dll"
			       "lspce-module.dll" t)
		    (copy-file "target/release/lspce_module.d"
			       "lspce-module.d" t))))
      (message "Done.")))
  (require 'lspce)
  :hook
  ((rust-mode . lspce-mode)
   (python-mode . lspce-mode)))

(use-package eldoc-box
  :hook
  ((lspce-mode . eldoc-box-hover-mode)))

(provide 'module-lspce)
