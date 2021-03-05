;;; rust.el - This is a configuration of the rust major mode.

;; commentary:
;; - This code doesen't depend on any dependencies.

;; Package that enables to comftably edit rust files.
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  :bind
  (("C-c C-c" . rust-run)))

(provide 'rust.el)
;;; rust.el ends here
