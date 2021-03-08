;;; org-mode.el - This is a configuration for emacs' org-mode.

;; commentary:
;; - The code in this file needs the following binaries:
;;   - dvipng (org-fragtog): delivered with MikTex/pdflatex
;;   - sqlite3 (org-roam): can be installed with pacman

;; Basic configuration of org-mode.
(use-package org-mode
  :init (org-babel-do-load-languages
	  'org-babel-load-languages
	  '((plantuml . t)
	    (python . t)
	    (shell . t)))
	 (when (window-system)
	   (setq org-ellipsis " ▼ "))
  :hook ((org-mode-hook . org-toggle-pretty-entities)
	 (org-mode-hook . prettify-symbols-mode)
	 (org-mode-hook . (lambda () (setq fill-column 70)))
	 (org-mode  . turn-on-auto-fill)
	 (org-mode . (lambda ()
		       (push '("[ ]" . "☐") prettify-symbols-alist)
		       (push '("[X]" . "☑" ) prettify-symbols-alist)
		       (push '("[-]" . "❍" ) prettify-symbols-alist)
		       (prettify-symbols-mode))))
  :custom ((org-startup-folded t)
	   (org-src-fontify-natively t)
	   (org-highlight-latex-and-related '(latex script entities))
	   (org-adapt-indentation nil)
	   (org-src-tab-acts-natively t)
	   (org-catch-invisible-edits 'smart)
	   (org-ctrl-k-protect-subtree t)
	   (initial-major-mode 'org-mode)
	   (prettify-symbols-unprettify-at-point 'right-edge)
	   (org-agenda-files (concat user-system-base-path "TODOs/TODOs.org"))
	   (org-latex-preview-ltxpng-directory "~/.ltxpng/")
	   (org-latex-packages-alist '(("AUTO" "babel" nil nil)
				       ("" "mhchem" t nil)))
	   (org-return-follows-link t)
	   (org-confirm-babel-evaluate nil)
	   (org-edit-src-content-indentation 0)
	   (org-src-preserve-indentation t)
	   (org-export-babel-evaluate t)))
				   
;; Function to collect headings to generate a TOC on pdf export.
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

;; Package to have bullet points.
(use-package org-superstar
  :ensure t
  :if window-system
  :hook (org-mode . (lambda () (org-superstar-mode 1)))
  :config (setq org-superstar-prettify-item-bullets t
		org-superstar-configure-like-org-bullets t
		org-hide-leading-stars nil
		org-superstar-leading-bullet ?\s
		org-superstar-special-todo-items t))

;; Package to google content in current buffer.
(use-package google-this
  :ensure t
  :config
  (google-this-mode 1)
  :bind ("C-x g" . google-this-mode-submap))

;; Package to have better syntax highlighting if exported to HTML.
(use-package htmlize
  :ensure t)

;; Package to calculate values within the org buffer.
(use-package literate-calc-mode
  :ensure t
  :hook (org-mode . literate-calc-minor-mode))   

;; Package to autotoggle LaTeX equations.
(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))

;; Package to generate RevealJS presentations.
(use-package ox-reveal
  :ensure t
  :custom ((org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
	   (org-reveal-mathjax t)
	   (org-reveal-ignore-speaker-notes nil)
	   (org-reveal-note-key-char nil)))

;; Package to put org files in a database.
(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory (concat user-system-base-path "Schule/"))
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c C-SPC C-f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

;; Package that provides a webinterface for org-roam.
(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20)
  :hook (early-init . org-roam-server-mode))

;; Package for async execution of source blocks.
(use-package ob-async
  :ensure t
  :after org)

;; Package to toggle =, /, _, etc.
(use-package org-appear
  :ensure t
  :quelpa (org-appear
	   :fetcher github
	   :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode)
  :init (setq org-hide-emphasis-markers t
	      org-appear-autoemphasis t
	      org-appear-autolinks t
	      org-appear-autosubmarkers t))

;; Package to journal in org-mode.
(use-package org-journal
  :ensure t
  :custom ((org-journal-file-type 'daily)
	   (org-journal-dir (concat user-system-base-path "Personal/Journal/"))
	   (org-journal-date-format "%A, %d. %B %Y")
	   (org-journal-file-header "#+TITLE: Daily Journal from %d.%m.%Y")
	   (org-journal-enable-agenda-integration t)))

;; Package to highlight the current heading in the header-line
(use-package org-sticky-header
  :ensure t
  :hook (org-mode . org-sticky-header-mode))

;; Default package to enable "<s TAB"
(use-package org-tempo)

;; Package to manage my templates.
(use-package snipsearch
  :ensure t
  :quelpa (snipsearch
	   :fetcher github
	   :repo "domse007/snipsearch")
  :custom ((snipsearch-list '(("org"
			       "#+TITLE: %1$s\n#+AUTHOR: %2$s\n#+OPTIONS: toc:t date:nil title:t author:t num:t \\n:t\n#+EXPORT_FILE_NAME:\n#+LATEX_CLASS: article\n#+LANGUAGE: de\n#+LATEX_HEADER: \\usepackage[AUTO]{babel}\n#+LATEX: \\setlength\\parindent{0pt}\n\n"
			       0)
			      ("eq" "\\[\\]" -2)
			      ("eqi" "\\(\\)" -2)
			      ("frac" "\\displaystyle\\frac{}{}" -3)
			      ("vec" "\\begin{pmatrix}  \\  \\  \\end{pmatrix}" -20)))
	   (snipsearch-author "Dominik Keller"))
  :bind ("C-c m" . snipsearch))

(provide 'org-mode.el)
;;; org-mode.el ends here
