;; (use-package cobol-mode
;;   :mode (("\\.cob\\'" . cobol-mode)
;; 	 ("\\.cbl\\'" . cobol-mode)
;; 	 ("\\.cpy\\'" . cobol-mode)))

(use-package cobol-superbol-mode
  :quelpa
  (cobol-superbol-mode
   :version original
   :fetcher url
   :url
   "https://github.com/OCamlPro/superbol-studio-oss/raw/master/emacs/cobol-superbol-mode.el")
  :mode (("\\.cob\\'" . cobol-mode)
	 ("\\.cbl\\'" . cobol-mode)
	 ("\\.cpy\\'" . cobol-mode)))

(use-package cobol-superbol-indent
  :quelpa
  (cobol-superbol-indent
   :version original
   :fetcher url
   :url
   "https://github.com/OCamlPro/superbol-studio-oss/raw/master/emacs/cobol-superbol-indent.el"))

(provide 'module-cobol)
