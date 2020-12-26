(global-set-key (kbd "C-c m") 'my-macros) ;; main function
(global-set-key (kbd "C-c §") 'insert-org) ;; std org template

(defun my-macros ()
  "Function to have a few personal shortcuts"
  (interactive)
  (setq my-input-macro (read-from-minibuffer "Macro ((org, orgtemplate), (pic, picture), frac, (eq, equation), (eqi, ieq), (vec, vector), center): ")) 
  ;; insert std org template
  (cond ((or (string-equal my-input-macro "stdtemplate")
	     (string-equal my-input-macro "orgtemplate")
	     (string-equal my-input-macro "org"))
	 (insert-org))
	;; insert fraction
	((string-equal my-input-macro "frac")            
	 (insert "\\displaystyle\\frac{}{}"))
	;; insert template for equation
	((or (string-equal my-input-macro "eq")
	     (string-equal my-input-macro "equation"))
	 (insert "\\[=\\]"))
	;; insert template to center stuff
	((string-equal my-input-macro "center")
	 (if (string-equal (buffer-substring-no-properties (line-beginning-position)
							   (line-end-position))
			   "")
	     (insert "\n") ;; then
	   (insert "\n\n")) ;; else
	 (insert "\\begin{center}\n\n"
		 "\\end{center}\n")
	 (dotimes (i 2) ;; for loop
	   (previous-line)))
	;; insert picture
	((or (string-equal my-input-macro "picture")
	     (string-equal my-input-macro "pic"))
	 (insert-org-picture))
	((or (string-equal my-input-macro "vec")
	     (string-equal my-input-macro "vector"))
	 (progn (insert "\\begin{pmatrix}  \\\\  \\\\  \\end{pmatrix}")
		(message "insert Numbers in whitespaces")))
	((or (string-equal my-input-macro "eqi") (string-equal my-input-macro "ieq"))
	 (insert "\\(=\\)"))
	;; else statement of cond -> if input is not a macro
	(t (message "Not a macro!"))))

(defun insert-org-picture ()
  "insert necessary things for a picture"
  (interactive)

  (let ((new-lines-pictures 0))
    (save-excursion
      (previous-line)
      (message (string (line-number-at-pos)))
      (if (string-equal (buffer-substring-no-properties (line-beginning-position)
							(line-end-position))
			"")
	  (progn 
	    (previous-line)
	    (when (not (string-equal (buffer-substring-no-properties (line-beginning-position)
								     (line-end-position))
				     ""))
	      (setq new-lines-pictures 1)))
	(setq new-lines-pictures 2)))
    (when (equal new-lines-pictures 1)
      (insert "\n"))
    (when (equal new-lines-pictures 2)
      (insert "\n\n")))
  
  (let ((actual-caption (read-string "Enter caption for picture: ")))
    (when (not (string-equal actual-caption ""))
      (insert "#+CAPTION: " actual-caption))
    (insert "\n#+ATTR_LATEX: :float nil :scale " 
	    (read-string "Custom scaling (empty: auto; arg: float>0): ")
	    "\n[[" (read-string "Location of the Picture: ./") "]]\n\n")
    (message "Inline Pictures can be enabled with C-c C-x C-v")))

(defun insert-org ()
  "Insert template for org mode"
  (interactive)
  (point-to-register ?r)
  (beginning-of-buffer)
  (insert "#+TITLE: " (read-string "Enter title of document (empty: buffername): " nil nil (if (string-equal (substring (buffer-name) -4) ".org")
											       (substring (buffer-name) 0 -4)
											     (buffer-name)))
	  "\n#+AUTHOR: Dominik Keller"
	  "\n#+OPTIONS: toc:t date:nil \\n:to"
	  "\n#+EXPORT_FILE_NAME: " (substring (buffer-name) 0 -4)
	  "\n#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup"
	  "\n#+REVEAL_ROOT: https://cdn.jsdelivr.net/npm/reveal.js"
	  "\n#+LATEX_CLASS: " )

  (setq latex-class-input (read-string "Enter Class (article, modern, book, report, selftemp): " nil nil "modern"))
  (if (string-equal latex-class-input "")
      (insert "modern")
    (if (or (string-equal "article" latex-class-input)
	    (string-equal "modern" latex-class-input)
	    (string-equal "book" latex-class-input)
	    (string-equal "report" latex-class-input)
	    (string-equal "selftemp" latex-class-input))
	(insert latex-class-input)
      (insert "modern")
      (message "Input not a defined class. Inserted stadard class.")))

  (insert "\n#+LANGUAGE: de"
	  "\n#+LATEX_HEADER: \\usepackage[AUTO]{babel}"
	  "\n#+LATEX: \\setlength\\parindent{0pt}"
	  "\n\n")
  (jump-to-register ?r)
  (message "Successfully inserted Org-Template"))

(defun open-config-file ()
  "open the config.org file"
  (interactive)
  (switch-to-buffer (find-file-noselect "~/.emacs.d/init.el" nil nil t))
  (goto-line 9))

(global-set-key (kbd "C-x RET RET") 'open-config-file)
