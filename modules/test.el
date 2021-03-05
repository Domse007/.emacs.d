(define-minor-mode org-gender
  "A minor mode to remind you to use gendered words.")

(defun gender ()
  (let* ((elem (thing-at-point 'word))
	 (char (substring elem 0 1)))
    (when (and (equal char (upcase char))
	       (equal (substring elem -2) "er"))
      (message "Hello World"))))

(add-hook 'post-command-hook #'gender)
