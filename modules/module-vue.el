(use-package typescript-mode)

(use-package treesit-auto
  :disabled t
  :hook (on-first-input . global-treesit-auto-mode)
  :custom (treesit-auto-install 'prompt)
  :config
  (add-to-list 'treesit-language-source-alist
	       `(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")))
  (add-to-list 'treesit-language-source-alist
	       `(tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")))
  (add-to-list 'treesit-language-source-alist
	       `(vue . ("https://github.com/ikatyang/tree-sitter-vue")))
  (add-to-list 'treesit-language-source-alist
	       `(css . ("https://github.com/tree-sitter/tree-sitter-css")))
  (add-to-list 'treesit-language-source-alist
	       `(scss . ("https://github.com/serenadeai/tree-sitter-scss"))))

(use-package vue-ts-mode
  :disabled t
  :quelpa
  (vue-ts-mode :fetcher github :repo "8uff3r/vue-ts-mode")
  :custom
  ((treesit-language-source-alist
    '((vue "https://github.com/ikatyang/tree-sitter-vue")
      (css "https://github.com/tree-sitter/tree-sitter-css")
      (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))))
  ;; (mapc #'treesit-install-language-grammar '(vue css typescript))
  )

(use-package vue-mode
  :config
  (custom-set-faces '(mmm-default-submode-face ((t nil)))))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(provide 'module-vue)
