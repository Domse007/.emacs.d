(defun kill-word-at-point ()
  "Kill the word where the point is pointing at."
  (interactive)
  (let ((range (bounds-of-thing-at-point 'symbol)))
    (kill-region (- (car range) 1) (cdr range))))

(global-set-key (kbd "M-k") 'kill-word-at-point)

(defun dk/delete-window ()
  "Wrapper around `delete-window' to balance windows
after deleting the active window."
  (interactive)
  (delete-window)
  (balance-windows))

(global-set-key (kbd "C-x 0") 'dk/delete-window)

;;------------------------------------------------------------------------------

(defconst dk/games '(gomoku tetris hanoi 5x5 blackbox bubbles dunnet life)
  "List of included games.")

(defun dk/play-a-game (&optional game)
  "Randomly play a game. The optional GAME argument specifies
a predefined game."
  (interactive)
  (if game
      (call-interactively game)
    (let* ((game-list-len (length dk/games))
	   (game-index (random game-list-len))
	   (game-to-be-played (nth game-index dk/games)))
      (call-interactively game-to-be-played))))

;;------------------------------------------------------------------------------

(require 'olivetti)

(defun disable-centering ()
  "Disable the centering. This is more or less an alias for 
olivetti mode."
  (interactive)
  (olivetti-mode nil))

;;------------------------------------------------------------------------------

(require 'auto-package-update)

(defun dk/package-update-packages ()
  "Update all packages. Alias for `auto-package-update-now'."
  (interactive)
  (message "Updating packages...")
  (auto-package-update-now))

;;------------------------------------------------------------------------------

(defun dk/config-version (&optional not-print only-version)
  "Return the major version of the config. If NOT-PRINT
is given, the function will not message the string. If
ONLY-VERSION is given, only the version string is returned
otherwise the whole sentence is returned."
  (interactive)
  (let ((version-string
	 (concat
	  (number-to-string dk/config-major-version)
	  "."
	  (number-to-string dk/config-minor-version)))
	(explanation "Personal Emacs config version: "))
    (when (not not-print)
      (message (concat explanation version-string)))
    (if only-version
	version-string
      (concat explanation version-string))))

;;------------------------------------------------------------------------------

(defun dk/explorer ()
  "Open the current directory in the file explorer."
  (interactive)
  (cond (dk/windows-flag (shell-command "explorer ."))
	(dk/linux-flag (shell-command "xdg-open ."))
	(t (message "This command is not supported on this platform."))))

;;------------------------------------------------------------------------------

(defun dk/count-loadable-files ()
  "Count the number of files in `dk/config-file-list'
that can be loaded. This is used by
`display-startup-echo-area-message'."
  (let ((counter 0))
    (dolist (elem dk/config-file-list)
      (let ((file (car elem))
	    (arg (cdr elem)))
	(when arg
	  (setq counter (+ counter 1)))))
    counter))

(defun dk/locate-config-init-error (index)
  "Function that tries to locate the file where an error occured.
INDEX is the index of the list `dk/config-file-list' where the
error occured."
  (let ((error-file (nth (+ index 2) dk/config-file-list)))
    (if (not (equal error-file nil))
	(car error-file)
      "Could not locate error.")))

(defun display-startup-echo-area-message ()
  "Redefining the default startup message function.
It appears to be very messi internally. Because
it's a redefine, it can't have the dk/ prefix."
  (let ((max-files (dk/count-loadable-files)))
    (progn 
      (message (concat "Info: Loaded "
		       (number-to-string dk/loaded-files-counter)
		       " files (out of "
		       (number-to-string max-files)
		       "). "
		       (number-to-string gcs-done)
		       " garbage collection runs. "
		       (if (equal max-files dk/loaded-files-counter)
			   "All loaded."
			 (concat "Error in file "
				 (dk/locate-config-init-error dk/loaded-files-counter)
				 "."))))
      (when (equal max-files dk/loaded-files-counter)
	(run-with-timer 2 nil 'dk/config-version)))))

(provide 'dk/custom-funcs)
