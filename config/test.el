(use-package org-sticky-header
  :ensure t
  :hook (org-mode . org-sticky-header-mode))

(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory (concat user-system-base-path "KantiBaden3Klasse/"))
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c C-SPC C-f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

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
        org-roam-server-network-label-wrap-length 20))

(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "Code: ")
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
				       ("#+caption:" . "☰")))

(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode)
