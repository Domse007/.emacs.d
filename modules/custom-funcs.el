(defun kill-word-at-point ()
  "Kill the word where the point is pointing at."
  (interactive)
  (let ((range (bounds-of-thing-at-point 'symbol)))
    (kill-region (- (car range) 1) (cdr range))))

(global-set-key (kbd "M-k") 'kill-word-at-point)

(defconst games '(gomoku tetris hanoi 5x5 blackbox bubbles dunnet life)
  "List of included games.")

(defun play-a-game ()
  "randomly play a game."
  (interactive)
  (let* ((game-list-len (length games))
	 (game-index (random game-list-len))
	 (game-to-be-played (nth game-index games)))
    (call-interactively game-to-be-played)))

(require 'olivetti)

(defun disable-centering ()
  "Disable the centering. This is more or less an alias for 
olivetti mode."
  (interactive)
  (olivetti-mode nil))

(require 'auto-package-update)

(defun package-update-packages ()
  (interactive)
  (auto-package-update-now))

(defun dk/config-version (&optional not-print)
  "Return the major version of the config."
  (interactive)
  (let ((version-string
	 (concat
	  (number-to-string dk/config-major-version)
	  "."
	  (number-to-string dk/config-minor-version))))
    (when (not not-print)
      (message (concat "Emacs config version: " version-string)))
    version-string))

(provide 'dk/custom-funcs)
