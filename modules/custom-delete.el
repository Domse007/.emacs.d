(defun kill-thing-at-point ()
  "Kill thing at point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (delete-region (car bounds) (+ (cdr bounds) 1))))

(global-set-key (kbd "M-k") 'kill-thing-at-point)

(provide 'custom-delete.el)
