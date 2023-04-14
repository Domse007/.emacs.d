(new-external-dependency!
 `(enchant . ,(if dk/windows-flag "pacman -S mingw-w64-x86_64-enchant"
	       "enchant"))
 t)

(use-package jinx
  :hook
  ((org-mode . jinx-mode))
  :bind
  (
   :map org-mode-map ("M-j" . jinx-correct)
   :map text-mode-map("M-j" . jinx-correct)))

(provide 'module-spell)
