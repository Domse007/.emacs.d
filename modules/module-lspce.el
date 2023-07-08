(use-package lspce
  :quelpa (lspce :fetcher github :repo "zbelial/lspce" :files ("*"))
  :init
  (let ((default-directory (file-name-directory (locate-library "lspce"))))
    (message "Entering %s" (pwd))
    (unless (or (file-exists-p "lspce-module.so")
                (file-exists-p "lspce-module.dll"))
      (shell-command "cargo build --release")
      (copy-file "target/release/liblspce_module.so" "lspce-module.so" t)
      (copy-file "target/release/liblspce_module.d" "lspce-module.d" t)
      (message "Done.")))
  (require 'lspce))

(provide 'module-lspce)
