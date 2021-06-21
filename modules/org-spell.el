(use-package ispell
  :config
  (setq-default ispell-program-name "aspell")
  :bind
  ("C-ö" . ispell-word)
  ("C-t" . ispell-word)
  :hook
  ((text-mode . flyspell-mode)
   (org-mode . flyspell-mode)))
			    
(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind
  (:map flyspell-mode-map
   ("C-$" . flyspell-correct-wrapper)))

(provide 'org-spell.el)
