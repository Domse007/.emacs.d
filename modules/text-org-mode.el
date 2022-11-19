;; org-transclusion

(new-external-dependency! 'pdflatex)
(new-external-dependency! 'dvipng)
(when dk/windows-flag
  ;; Make more obvious what to install.
  (new-external-dependency! 'miktex))

(use-package org
  :pin melpa
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)
     (python . t)
     (shell . t)))
  ;; (when (window-system)
  ;;   (setq org-ellipsis " ▼ "))
  :hook
  ((org-mode-hook . org-toggle-pretty-entities)
   (org-mode-hook . prettify-symbols-mode)
   (org-mode-hook . (lambda () (setq fill-column 70)))
   (org-mode  . turn-on-auto-fill)
   ;; (org-mode . (lambda ()
   ;; 		 (push '("[ ]" . "☐") prettify-symbols-alist)
   ;; 		 (push '("[X]" . "☑" ) prettify-symbols-alist)
   ;; 		 (push '("[-]" . "❍" ) prettify-symbols-alist)
   ;; 		 (prettify-symbols-mode)))
   )
  :custom
  ((org-src-fontify-natively t)
   (org-highlight-latex-and-related '(latex script entities))
   (org-startup-with-latex-preview t)
   (org-adapt-indentation nil)
   (org-src-tab-acts-natively t)
   (org-catch-invisible-edits 'smart)
   (org-ctrl-k-protect-subtree t)
   (prettify-symbols-unprettify-at-point 'right-edge)
   (org-agenda-files `(,(concat dk/user-system-base-path "TODOs.org")))
   (org-latex-preview-ltxpng-directory "~/.ltxpng/")
   (org-latex-packages-alist '(("AUTO" "babel" nil nil)
			       ("" "mhchem" t nil)))
   (org-return-follows-link t)
   (org-confirm-babel-evaluate nil)
   (org-edit-src-content-indentation 0)
   (org-src-preserve-indentation t)
   (org-export-babel-evaluate t)
   (org-id-locations-file
    (concat dk/user-emacs-cache-dir "org/.org-id-locations"))
   (org-export-allow-bind-keywords t)
   (org-image-actual-width nil)
   (org-special-ctrl-a/e t))
  :hook
  ((org-mode-hook . (lambda () (linum-mode nil)))))

;; (use-package org-superstar
;;   :defer t
;;   :if (window-system)
;;   :hook
;;   ((org-mode-hook . (lambda () (org-superstar-mode t)))
;;    (org-mode . org-superstar-mode))
;;   :custom
;;   ((org-superstar-prettify-item-bullets t)
;;    (org-superstar-configure-like-org-bullets t)
;;    (org-hide-leading-stars nil)
;;    (org-superstar-leading-bullet ?\s)
;;    (org-superstar-special-todo-items t)))

(use-package htmlize
  :defer t)

(use-package org-fragtog
  :defer t
  :if (window-system)
  :hook
  ((org-mode . org-fragtog-mode)))

(use-package org-appear
  :defer t
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

;; (use-package ox-reveal
;;   :defer t
;;   :custom
;;   ((org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
;;    (org-reveal-mathjax t)
;;    (org-reveal-ignore-speaker-notes nil)
;;    (org-reveal-note-key-char nil)))

;; (dk/get-package!
;;     :user "Domse007"
;;     :repo "snipsearch"
;;     :force dk/get-package-override-git-availability)

;; (use-package snipsearch
;;   :after helm
;;   ;;:ensure (not dk/get-package-override-git-availability)
;;   :quelpa
;;   (snipsearch :fetcher github :repo "domse007/snipsearch")
;;   :custom
;;   ((snipsearch-list
;;     `(("org" ,(concat "#+TITLE: %1$s\n"
;; 		      "#+AUTHOR: %2$s\n"
;; 		      "#+OPTIONS: toc:t date:nil title:t author:t num:t \\n:t\n"
;; 		      "#+EXPORT_FILE_NAME:\n"
;; 		      "#+LATEX_CLASS: article\n"
;; 		      "#+LANGUAGE: de\n"
;; 		      "#+LATEX_HEADER: \\usepackage[AUTO]{babel}\n"
;; 		      "#+LATEX: \\setlength\\parindent{0pt}\n\n"))
;;       ("eq" "\\[\\]" -2)
;;       ("eqi" "\\(\\)" -2)
;;       ("frac" "\\displaystyle\\frac{}{}" -3)
;;       ("vec" "\\begin{pmatrix}  \\\\  \\\\  \\end{pmatrix}" -20)
;;       ("sys" "\\begin{Bmatrix}  \\\\  \\\\  \\end{Bmatrix}" -20)
;;       ("ce" "\\ce{}" -1)
;;       ("ceq" "\\[\\ce{}\\]" -3)
;;       ("ceqi" "\\(\\ce{}\\)" -3)
;;       ("arr" "\\(\\rightarrow\\) " 0)))
;;    (snipsearch-author "Dominik Keller"))
;;   :bind
;;   (("C-c m" . snipsearch)))

;; (dk/get-package!
;;     :user "Fuco1"
;;     :repo "org-pretty-table"
;;     :force dk/get-package-override-git-availability)

;; (use-package org-pretty-table
;;   :ensure (not dk/get-package-override-git-availability)
;;   :quelpa
;;   (org-pretty-table :fetcher github :repo "Fuco1/org-pretty-table")
;;   :hook
;;   ((org-mode . org-pretty-table-mode)))

(use-package org-modern
  :hook
  ((org-mode . org-modern-mode)))

(new-external-dependency! 'gnupg) ;; pacman -S mingw-w32-x86_64-gnupg

(use-package org-crypt)

(provide 'text-org-mode)
