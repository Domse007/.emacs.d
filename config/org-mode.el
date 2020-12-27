(use-package org-mode
  :hook ((org-mode-hook . org-toggle-pretty-entities)
	 (org-mode-hook . prettify-symbols-mode)
	 (org-mode . (lambda () (setq fill-column 70)))
	 (org-mode  . turn-on-auto-fill))
  :custom ((org-startup-folded t)
	   (org-src-fontify-natively t)
	   (org-adapt-indentation nil)
	   (org-src-tab-acts-natively t)
	   (initial-major-mode 'org-mode)
	   (org-hide-emphasis-markers t)
	   (org-ellipsis " ▼ ")
	   (org-agenda-files '(when (concat user-system-base-path "TODOs/")))
	   (prettify-symbols-unprettify-at-point 'right-edge))
  :config (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "Code: ")
						 ("#+END_SRC" . "Code;")
						 ("#+begin_src" . "Code: ")
						 ("#+end_src" . "Code;")
						 ("#+TITLE:" . "Title: ")
						 ("#+title:" . "Title: ")
						 ("#+SUBTITLE:" . "Subtitle: ")
						 ("#+subtitle:" . "Subtitle: ")
						 ("#+DATE:" . "Date: ")
						 ("#+date:" . "Date: ")
						 ("#AUTHOR:" . "Author: ")
						 ("#author:" . "Author: ")
						 ("#+PROPERTY:" . "Property: ")
						 ("#+property:" . "Property: ")
						 ("#+OPTIONS:" . "Options: ")
						 ("#+options:" . "Options")
						 ("#+LATEX_HEADER:" . "LaTeX-Header: ")
						 ("#+latex_header:" . "LaTeX-Header: ")
						 ("#+LATEX_CLASS:" . "LaTeX-Class: ")
						 ("#+latex_class:" . "LaTeX-Class: ")
						 ("#+ATTR_LATEX:" . "LaTeX-Attribute: ")
						 ("#+attr_latex:" . "LaTeX-Attribute: ")
						 ("#+LATEX:" . "LaTeX: ")
						 ("#+latex:" . "LaTeX: ")
						 ("#+ATTR_HTML:" . "HTML-Attribute: ")
						 ("#+attr_html:" . "HTML-Attribute: ")
						 ("#+BEGIN_QUOTE:" . "❮❮")
						 ("#+begin_quote:" . "❮❮")
						 ("#+END_QUOTE:" . "❯❯")
						 ("#+end_quote:" . "❯❯")
						 ("#+CAPTION:" . "☰")
						 ("[ ]" . "☐")
						 ("[X]" . "☑")
						 ("[-]" . "❍"))))
				     
(defun org-export-collect-headlines (info &optional n)
"Collect headlines in order to build a table of contents. [...]
  Return a list of all exportable headlines as parsed elements.
  Footnote sections, if any, will be ignored."
(let ((limit (plist-get info :headline-levels)))
  (setq n (if (wholenump n) (min n limit) limit))
  (org-element-map (plist-get info :parse-tree) 'headline
    #'(lambda (headline)
        (unless (or (org-element-property :NOTOC headline)               ; new condition
                    (org-element-property :footnote-section-p headline)) ; old condition
          (let ((level (org-export-get-relative-level headline info)))
            (and (<= level n) headline))))
    info)))
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
	       '("modern"
		 "\\documentclass{scrartcl}
                \\usepackage{microtype}
                \\usepackage{tgpagella}
                \\usepackage[scale=.9]{tgheros}
                \\usepackage{tgcursor}
                \\usepackage{paralist}
                \\newcommand{\\rc}{$^{14}C$}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package org-superstar
  :ensure t
  :hook (org-mode . (lambda () (org-superstar-mode 1)))
  :config (setq org-superstar-prettify-item-bullets t
		org-superstar-configure-like-org-bullets t
		org-hide-leading-stars nil
		org-superstar-leading-bullet ?\s
		org-superstar-special-todo-items t))

(use-package google-this
  :ensure t
  :config
  (google-this-mode 1)
  :bind ("C-x g" . google-this-mode-submap))

(use-package htmlize
  :ensure t)

(use-package literate-calc-mode
  :ensure t
  :hook (org-mode . literate-calc-minor-mode))   

(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))

(use-package ox-reveal
  :ensure t
  :custom ((org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
	   (org-reveal-mathjax t)
	   (org-reveal-ignore-speaker-notes nil)
	   (org-reveal-note-key-char nil)))
