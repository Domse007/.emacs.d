(new-external-dependency! 'python)
(new-external-dependency! 'pip)
(new-external-dependency! '(epc . "pip install epc"))

(use-package lsp-bridge
  :quelpa
  (lsp-bridge :fetcher github :repo "manateelazycat/lsp-bridge")
  :config
  (global-lsp-bridge-mode))

(provide 'programming-lsp-bridge)
