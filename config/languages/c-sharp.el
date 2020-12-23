(use-package omnisharp
  :ensure t
  :init (omnisharp-mode)
  :hook (csharp-mode . omnisharp-mode)
  :bind (("C-c C-c" . recompile)
	 ("C-c r r" . omnisharp-run-code-action-refactoring))
  :config (eval-after-load
	      'company
	    '(add-to-list 'company-backends #'company-omnisharp)
	    )
  :hook (csharp-mode-hook . flycheck-mode))
