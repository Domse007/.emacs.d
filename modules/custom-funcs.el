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

(defconst games '(gomoku tetris hanoi 5x5 blackbox bubbles dunnet life)
  "List of included games.")

(defun play-a-game ()
  "randomly play a game."
  (interactive)
  (let* ((game-list-len (length games))
	 (game-index (random game-list-len))
	 (game-to-be-played (nth game-index games)))
    (call-interactively game-to-be-played)))

(defun disable-centering ()
  "Disable the centering. This is more or less an alias for 
olivetti mode."
  (interactive)
  (olivetti-mode nil))

(provide 'custom-funcs.el)
