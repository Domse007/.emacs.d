(use-package snipsearch
  :ensure t
  :quelpa (snipsearch
	   :fetcher github
	   :repo "domse007/snipsearch")
  :custom ((snipsearch-list '(("org"
			       "#+TITLE: %1$s\n#+AUTHOR: %2$s\n#+OPTIONS: toc:t date:nil title:t author:t num:t \\:t\n#+EXPORT_FILE_NAME:\n#+LATEX_CLASS: article\n#+LANGUAGE: de\n#+LATEX_HEADER: \\usepackage[AUTO]{babel}\n#+LATEX: \\setlength\\parindent{0pt}\n\n"
			       0)
			      ("eq" "\\[\\]" -2)
			      ("eqi" "\\(\\)" -2)
			      ("frac" "\\displaystyle\\frac{}{}" -3)
			      ("vec" "\\begin{pmatrix}  \\  \\  \\end{pmatrix}" -20)))
	   (snipsearch-author "Dominik Keller"))
  :bind ("C-c m" . snipsearch))

(use-package ispell
  :config
  (when (not (executable-find "aspell"))
    (progn (message "aspell not present. Trying to install it.")
	   (when (string-equal system-type "windows-nt")
	     (if (executable-find "pacman")
		 (progn (shell-command "pacman -S mingw64/mingw-w64-x86_64-aspell")
			(shell-command "pacman -S mingw64/mingw-w64-x86_64-aspell-de")
			(message "aspell and aspell-de not present. Installing..."))
	       (message "Can't find pacman. Maybe install MSYS2 or add to path.")))))
  (setq-default ispell-program-name "aspell")
  (ispell-change-dictionary "de" t)
  
  :bind ("C-ö" . ispell-word)
  :hook ((text-mode . flyspell-mode)
	 (org-mode . flyspell-mode)
	 (org-mode . (lambda ()
		       (interactive)
		       (save-excursion
			 (goto-char (word-search-forward "#+LANGUAGE: "))
			 (ispell-change-dictionary (thing-at-point 'word)))))))
