(defun kill-word-at-point ()
  (interactive)
  (let ((range (bounds-of-thing-at-point 'symbol)))
    (kill-region (- (car range) 1) (cdr range))))

(global-set-key (kbd "M-k") 'kill-word-at-point)

(defun programming-mode ()
  "Infortunately `treemacs' is really hard to enable with other 
minor modes at the same time. That's why I need this helper function.
In a perfect world `treemacs' would take an arg."
  (interactive)
  (let ((buffer-name (current-buffer)))
    (when (not (equal (treemacs-current-visibility) 'visible))
      (treemacs))
    (switch-to-buffer buffer-name)
    (delete-other-windows)
    (display-line-numbers-mode t)))

(defun disable-programming-mode ()
  "This is the counterpart to `programming-mode'."
  (interactive)
  (let ((buffer-name (current-buffer)))
    (when (equal (treemacs-current-visibility) 'visible)
      (treemacs))
    (switch-to-buffer buffer-name))
  (lambda () (display-line-numbers-mode nil)))

(add-hook 'org-mode-hook 'disable-programming-mode)

(provide 'custom-funcs.el)
