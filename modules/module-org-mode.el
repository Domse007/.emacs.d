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
   '((emacs-lisp . t)(plantuml . t)(python . t)(shell . t)(C . t)))
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
   (org-mode . company-mode))
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
   ;; (org-latex-listings 'minted)
   (org-latex-src-block-backend 'engraved)
   (org-latex-packages-alist '(("AUTO" "babel"  nil nil)
			       (""     "mhchem" t   nil)))
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
  ;; ((org-mode-hook . (lambda () (linum-mode nil))))
  )

(defun dk/org-latex-fragments-scaling ()
  (interactive)
  (let* ((current (plist-get org-format-latex-options :scale))
	 (new (read-number (format "New scaling (current %s): " current) 1.0)))
    (plist-put org-format-latex-options :scale new)))

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

;; For setting org-latex-src-block-backend
(use-package engrave-faces)

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

(use-package org-tidy
  ;; Hide :PROPERTIES: from the buffer.
  :hook
  ((org-mode . org-tidy-mode)))

(use-package org-modern
  :hook
  ((org-mode . org-modern-mode)))

(new-external-dependency! 'gnupg) ;; pacman -S mingw-w32-x86_64-gnupg

(provide 'module-org-mode)
