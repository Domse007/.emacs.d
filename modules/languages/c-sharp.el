;;; c-sharp.el - This is a providing a better experience programming in c-sharp.

;; commentary:
;; - This code depends on the omnisharp LSP binaries.

;; Package that provides an omnisharp interace.
(use-package omnisharp
  :ensure t
  :hook (csharp-mode . omnisharp-mode)
  :bind (("C-c C-c" . recompile)
	 ("C-c r r" . omnisharp-run-code-action-refactoring))
  :config (eval-after-load
	      'company
	    '(add-to-list 'company-backends #'company-omnisharp))
  :hook ((csharp-mode . flycheck-mode)
	 (csharp-mode . omnisharp-start-omnisharp-server)))

(provide 'c-sharp.el)
;;; c-sharp.el ends here.
