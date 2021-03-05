;;; spell.el - This is a configuration for spell checking.

;; commentary:
;; - The Code in this file depends on aspell

;; Package that makes Emacs to a aspell client.
(use-package ispell
  :config
  (setq-default ispell-program-name "aspell")
  (ispell-change-dictionary "de" t)
  :bind ("C-ö" . ispell-word)
  :hook ((text-mode . flyspell-mode)
	 (org-mode . flyspell-mode)
	 (org-mode . (lambda ()
		       (interactive)
		       (let ((dk/position 0))
			 (save-excursion
			   (goto-char 0)
			   (setq dk/position (word-search-forward "#+LANGUAGE: " nil t))
			   (if (not (equal (point) 0))
			       (progn (goto-char dk/position)
				      (ispell-change-dictionary (thing-at-point 'word))
				      (message "No language recognized!")))))))))

;; Package that enables autocorrecting spellmistakes.
(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-ü" . flyspell-correct-wrapper))
;;  :hook ((org-mode . flyspell-correct-auto-mode)
;;	 (text-mode . flyspell-correct)))
)

;; Package that enables ivy integration for flyspell.
(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell-correct)

;; Package that provides a popup for flyspell.
(use-package flyspell-correct-popup
  :ensure t
  :after flyspell-correct
  :bind (:map popup-menu-keymap
              ("TAB" . popup-next)
              ("S-TAB" . popup-previous)))
(setq flyspell-correct-interface 'break)

(provide 'spell.el)
;;; spell.el ends here
