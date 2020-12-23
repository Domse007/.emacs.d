(use-package emmet-mode
  :ensure t
  :hook ((css-mode . emmet-mode)
	 (html-mode . emmet-mode)))

(use-package js2-mode
  :ensure t)

(use-package company-web
  :ensure t
  :config (require 'company-web-html)
  (defun my-web-mode-hook ()
    (set (make-local-variable 'company-backends) '(company-css company-web))
    )
  ;; Enable JavaScript completion between <script>...</script> etc.
  (advice-add 'company-tern :before
              #'(lambda (&rest _)
                  (if (equal major-mode 'web-mode)
                      (let ((web-mode-cur-language
                             (web-mode-language-at-pos)))
			(if (or (string= web-mode-cur-language "javascript")
				(string= web-mode-cur-language "jsx"))
                            (unless tern-mode (tern-mode))
                          (if tern-mode (tern-mode -1))))))))
