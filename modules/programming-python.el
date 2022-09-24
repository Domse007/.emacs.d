(new-external-dependency! 'python)

(use-package python-mode
  :defer t
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy))

;; (use-package 'dap-python)

(use-package pyvenv
  :defer t
  :config
  (pyvenv-mode 1))

(use-package lsp-python-ms
  :defer t
  :after lsp)

(provide 'programming-python)
