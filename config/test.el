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

(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "λ")
                                       ("#+END_SRC" . "λ")
                                       ("#+begin_src" . "λ")
                                       ("#+end_src" . "λ")
				       ("#+TITLE:" . "𝙏")
				       ("#+title:" . "𝙏")
				       ("#+SUBTITLE:" . "𝙩")
				       ("#+subtitle:" . "𝙩")
				       ("#+DATE:" . "𝘿")
				       ("#+date:" . "𝘿")
				       ("#AUTHOR:" . "𝘼")
				       ("#author:" . "𝘼")
				       ("#+PROPERTY:" . "☸")
				       ("#+property:" . "☸")
				       ("#+OPTIONS:" . "⌥")
				       ("#+options:" . "⌥")
				       ("#+LATEX_HEADER:" . "⇾")
				       ("#+latex_header:" . "⇾")
				       ("#+LATEX_CLASS:" . "⇥")
				       ("#+latexx_class:" . "⇥")
				       ("#+ATTR_LATEX:" . "🄛")
				       ("#+attr_latex:" . "🄛")
				       ("#+LATEX:" . "ɫ")
				       ("#+latex:" . "ɫ")
				       ("#+ATTR_HTML:" . "🄗")
				       ("#+attr_html:" . "🄗")
				       ("#+BEGIN_QUOTE:" . "❮")
				       ("#+begin_quote:" . "❮")
				       ("#+END_QUOTE:" . "❯")
				       ("#+end_quote:" . "❯")
				       ("#+CAPTION:" . "☰")
				       ("#+caption:" . "☰")
                                       ))

(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode)
