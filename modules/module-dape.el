;; TODO: actually test it, as the current pop-os release has no gdb-14...
(use-package dape
  :preface
  (setq dape-key-prefix "\C-x\C-a")
  :config
  (dape-breakpoint-global-mode)
  (setq dape-buffer-window-arrangement 'right)
  (setq dape-inlay-hints t)
  (setq dape-cwd-function 'projectile-project-root))

(provide 'module-dape)
