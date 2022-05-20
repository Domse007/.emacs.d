(require 'json)

(defconst dk/get-package-github-api-url "https://api.github.com/repos/"
  "Default url where the packages will be pulled from.")

(defconst dk/get-package-github-content-url "https://raw.githubusercontent.com/")

(defcustom dk/get-package-branch-names '("master" "main" "release")
  "List of branch names that are acceptable to pull. The order is the order in
which they are checked if they can be used.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; URL Builders                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dk/get-package-build-url-for-branch-list (user repo)
  "Build an url string that can be used to get a list of all branches."
  (let ((url dk/get-package-github-api-url))
    (concat url user "/" repo "/branches")))

(defun dk/get-package-build-url-for-file-list (user repo)
  "Build an url string that can be used to get a list of files in a repo."
  (let ((url dk/get-package-github-api-url)
	(branch (dk/get-package-find-branch)))
    (concat url user "/" repo "/git/trees/" branch "?recursive=1")))

(defun dk/get-package-build-url-for-content (user repo branch path)
  "Build an url string that can be used to get file content from github."
  (let ((url dk/get-package-github-content-url))
    (concat url "/" user "/" repo "/" branch "path")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Content getters                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dk/get-package-retrieve-content-and-save (url file-name)
  "Get content from url and return it as a string."
  (with-temp-buffer
    (url-insert-file-contents url)
    (write-file file-name nil)))

(defun dk/get-package-retrieve-content-as-json (url)
  "Get content from url and return it as a string."
  (with-temp-buffer
    (url-insert-file-contents url)
    (buffer-string)
    (let ((json-false :false))
      (json-read))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extractors                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dk/get-package-get-branches (user repo)
  "Get a list of all branches from a `repo' by `user'"
  (let* ((url (dk/get-package-build-url-for-branch-list user repo))
	 (content (dk/get-package-retrieve-content-as-json url)))
    (mapcar (lambda (elem) (cdr (car elem))) content)))

(defun dk/get-packages-select-branch (available-branches)
  "Select a branch that will used to get the files from github."
  (let ((selectable-branches (mapcar (lambda (elem)
				       (member elem dk/get-package-branch-names))
				     available-branches)))
    (if (eq selectable-branches nil)
	(error "There are no fitting branches.")
      (car selectable-branches))))

(defun dk/get-packages-get-file-names (url)
  "Get all files from a repo from github."
  (let* ((content (dk/get-package-retrieve-content-as-json url))
	 (tree (nth 2 content)))
    (mapcar (lambda (elem)
	      (let ((keyword (car (nth 0 elem)))
		    (file (cdr (nth 0 elem))))
		(when (string-equal keyword "path")
		  file)))
	    (cdr tree))))

(provide 'base-get-package)
