(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  :bind
  (("C-c C-c" . rust-run)))
