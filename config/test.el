(use-package org-sticky-header
  :ensure t
  :hook (org-mode . org-sticky-header-mode))

;; (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "Code: ")
;; 				       ("#+END_SRC" . "Code;")
;; 				       ("#+begin_src" . "Code: ")
;; 				       ("#+end_src" . "Code;")
;; 				       ("#+TITLE:" . "Title: ")
;; 				       ("#+title:" . "Title: ")
;; 				       ("#+SUBTITLE:" . "Subtitle: ")
;; 				       ("#+subtitle:" . "Subtitle: ")
;; 				       ("#+DATE:" . "Date: ")
;; 				       ("#+date:" . "Date: ")
;; 				       ("#AUTHOR:" . "Author: ")
;; 				       ("#author:" . "Author: ")
;; 				       ("#+PROPERTY:" . "Property: ")
;; 				       ("#+property:" . "Property: ")
;; 				       ("#+OPTIONS:" . "Options: ")
;; 				       ("#+options:" . "Options")
;; 				       ("#+LATEX_HEADER:" . "LaTeX-Header: ")
;; 				       ("#+latex_header:" . "LaTeX-Header: ")
;; 				       ("#+LATEX_CLASS:" . "LaTeX-Class: ")
;; 				       ("#+latex_class:" . "LaTeX-Class: ")
;; 				       ("#+ATTR_LATEX:" . "LaTeX-Attribute: ")
;; 				       ("#+attr_latex:" . "LaTeX-Attribute: ")
;; 				       ("#+LATEX:" . "LaTeX: ")
;; 				       ("#+latex:" . "LaTeX: ")
;; 				       ("#+ATTR_HTML:" . "HTML-Attribute: ")
;; 				       ("#+attr_html:" . "HTML-Attribute: ")
;; 				       ("#+BEGIN_QUOTE:" . "❮❮")
;; 				       ("#+begin_quote:" . "❮❮")
;; 				       ("#+END_QUOTE:" . "❯❯")
;; 				       ("#+end_quote:" . "❯❯")
;; 				       ("#+CAPTION:" . "☰")
;; 				       ("[ ]" . "☐")
;; 				       ("[X]" . "☑")
;; 				       ("[-]" . "❍"))))

;; (setq prettify-symbols-unprettify-at-point 'right-edge)
;; (add-hook 'org-mode-hook 'prettify-symbols-mode)


;; ;; spell checking
;; ;; (setq ispell-program-name "hunspell")
;; (setq ispell-hunspell-dict-paths-alist
;;       '(("en_US" "c:/Users/Dominik/AppData/Roaming/.emacs.d/config/languages/dict/en_US.aff")
;;         ("de_DE" "c:/Users/Dominik/AppData/Roaming/.emacs.d/config/languages/dict/de_DE.aff")))

;; (setq ispell-personal-dictionary "de_DE")

;; (setq ispell-local-dictionary "de_DE")
;; (setq ispell-local-dictionary-alist
;;       '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
;;         ("de_DE" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "de_DE" "-a" "-i" "UTF-8") nil utf-8)))

;; (flyspell-mode 1)
;; (add-hook 'text-mode-hook #'flyspell-mode)
;; (add-hook 'org-mode-hook #'flyspell-mode)

;; (global-set-key (kbd "C-.") 'ispell-word)

