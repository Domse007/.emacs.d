(defun kill-word-at-point ()
  (interactive)
  (let ((range (bounds-of-thing-at-point 'symbol)))
    (kill-region (- (car range) 1) (cdr range))))

(global-set-key (kbd "M-k") 'kill-word-at-point)

(defun dk/kill-line ()
  (interactive)
  (kill-line)
  (kill-line))

(global-set-key (kbd "C-k") 'dk/kill-line)

(provide 'custom-funcs.el)
