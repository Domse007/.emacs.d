(use-package python-mode
  :hook
  ((python-mode . lsp-deferred))
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy))

;; (use-package 'dap-python)

(use-package pyvenv
  :config
  (pyvenv-mode 1))

;; (use-package lsp-python-ms
;;   :after lsp)

(provide 'dk/programming-python)
