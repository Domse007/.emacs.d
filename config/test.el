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
