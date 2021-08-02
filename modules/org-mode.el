(use-package org-mode
  :straight t
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)
     (python . t)
     (shell . t)))
  (when (window-system)
    (setq org-ellipsis " ▼ "))
  :hook
  ((org-mode-hook . org-toggle-pretty-entities)
   (org-mode-hook . prettify-symbols-mode)
   (org-mode-hook . (lambda () (setq fill-column 70)))
   (org-mode  . turn-on-auto-fill)
   (org-mode . (lambda ()
		 (push '("[ ]" . "☐") prettify-symbols-alist)
		 (push '("[X]" . "☑" ) prettify-symbols-alist)
		 (push '("[-]" . "❍" ) prettify-symbols-alist)
		 (prettify-symbols-mode))))
  :custom
  ((org-src-fontify-natively t)
   (org-highlight-latex-and-related '(latex script entities))
   (org-startup-with-latex-preview t)
   (org-adapt-indentation nil)
   (org-src-tab-acts-natively t)
   (org-catch-invisible-edits 'smart)
   (org-ctrl-k-protect-subtree t)
   (initial-major-mode 'org-mode)
   (prettify-symbols-unprettify-at-point 'right-edge)
   (org-agenda-files (concat dk/user-system-base-path "/TODOs/TODOs.org"))
   (org-latex-preview-ltxpng-directory "~/.ltxpng/")
   (org-latex-packages-alist '(("AUTO" "babel" nil nil)
			       ("" "mhchem" t nil)))
   (org-return-follows-link t)
   (org-confirm-babel-evaluate nil)
   (org-edit-src-content-indentation 0)
   (org-src-preserve-indentation t)
   (org-export-babel-evaluate t)))

(use-package org-superstar
  :straight t
  :if (window-system)
  :hook
  ((org-mode . (lambda () (org-superstar-mode t))))
  :custom
  ((org-superstar-prettify-item-bullets t)
   (org-superstar-configure-like-org-bullets t)
   (org-hide-leading-stars nil)
   (org-superstar-leading-bullet ?\s)
   (org-superstar-special-todo-items t)))

(use-package htmlize
  :straight t)

(use-package org-fragtog
  :straight t
  :if (window-system)
  :hook
  ((org-mode . org-fragtog-mode)))

(use-package org-appear
  :straight t
  :if (window-system)
  :hook
  (org-mode . org-appear-mode)
  :custom
  ((org-hide-emphasis-markers t)
   (org-appear-autoemphasis t)
   (org-appear-autolinks t)
   (org-appear-autosubmarkers t)))

(use-package org-tempo
  :ensure nil)

(use-package ox-reveal
  :straight t
  :custom
  ((org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
   (org-reveal-mathjax t)
   (org-reveal-ignore-speaker-notes nil)
   (org-reveal-note-key-char nil)))

;; (use-package snipsearch
;;   :after helm
;;   :quelpa
;;   (snipsearch
;;    :fetcher github
;;    :repo "domse007/snipsearch")
;;   :custom
;;   ((snipsearch-list
;;     '(("org"
;;        "#+TITLE: %1$s\n#+AUTHOR: %2$s\n#+OPTIONS: toc:t date:nil title:t author:t num:t \\n:t\n#+EXPORT_FILE_NAME:\n#+LATEX_CLASS: article\n#+LANGUAGE: de\n#+LATEX_HEADER: \\usepackage[AUTO]{babel}\n#+LATEX: \\setlength\\parindent{0pt}\n\n"
;;        0)
;;       ("eq" "\\[\\]" -2)
;;       ("eqi" "\\(\\)" -2)
;;       ("frac" "\\displaystyle\\frac{}{}" -3)
;;       ("vec" "\\begin{pmatrix}  \\\\  \\\\  \\end{pmatrix}" -20)
;;       ("sys" "\\begin{Bmatrix}  \\\\  \\\\  \\end{Bmatrix}" -20)
;;       ("ce" "\\ce{}" -1)
;;       ("ceq" "\\[\\ce{}\\]" -3)
;;       ("ceqi" "\\(\\ce{}\\)" -3)
;;       ("arr" "\\(\\rightarrow\\) " 0)))
;;    (snipsearch-author "Dominik Keller")
;;    (snipsearch-comp-interface 'helm))
;;   :bind
;;   (("C-c m" . snipsearch)))

;; (use-package org-lang
;;   :quelpa
;;   (org-lang
;;    :repo domse007/org-lang
;;    :fetcher github)
;;   :init
;;   (use-package fuzzy)
;;   :custom
;;   ((org-lang-fallback-lang "de_CH")
;;    (org-lang-installed-langs
;;     '("de_CH" "de_DE" "fr_CH" "en_US"))
;;    (org-lang-prefered-completion 'helm)
;;    (org-lang-check-after-enable t))
;;   :hook
;;   ((org-mode . org-lang-mode)
;;    (org-mode . org-lang-get-buffer-lang)))

(provide 'org-mode.el)
