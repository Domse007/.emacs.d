(use-package evil
  :config
  ;; (evil-mode t)
  )

;; (use-package evil-general)

(use-package evil-org
  :after org
  :hook
  (org-mode . (lambda () evil-org-mode)))

(provide 'base-evil)
