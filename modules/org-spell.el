(use-package ispell
  :if dk/windows-flag
  :config
  (setq-default ispell-program-name "hunspell")
  :custom
  ispell-local-dictionary-alist
  '(("english" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)
    ("german" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "de_CH") nil utf-8)
    ("french" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "fr") nil utf-8))
  :bind
  ("C-ö" . ispell-word)
  ("C-t" . ispell-word)
  :hook
  ((text-mode . flyspell-mode)
   (org-mode . flyspell-mode)))

(use-package flyspell-correct
  :if dk/windows-flag
  :after flyspell
  :bind
  (:map flyspell-mode-map
	("C-$" . flyspell-correct-wrapper)))

(use-package fuzzy)

(use-package org-lang
  :disabled t
	:if dk/windows-flag
  :quelpa (org-lang :fetcher github :repo "domse007/org-lang")
  :init
  (use-package fuzzy)
  :custom
  ((org-lang-fallback-lang "de_CH")
   (org-lang-installed-langs
    '("de_CH" "de_DE" "fr_CH" "en_US"))
   (org-lang-check-after-enable t))
  :hook
  ((org-mode . org-lang-mode)
   (org-mode . org-lang-get-buffer-lang)))

(provide 'dk/org-spell)
