;;; web-dev.el - Configuration to program for web apps.

;; commentary:
;; - This code doesen't depend on any dependencies.

;; Package that provides emmet for emacs.
(use-package emmet-mode
  :ensure t
  :hook ((css-mode . emmet-mode)
	 (html-mode . emmet-mode)))

;; Package thate enables improved JS editing.
(use-package js2-mode
  :ensure t)

;; Package that enables autocompletion in web modes.
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

(provide 'web-dev.el)
;;; web-dev.el ends here
