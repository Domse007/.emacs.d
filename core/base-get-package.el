(require 'json)

(defconst dk/get-package-github-api-url "https://api.github.com/repos/"
  "Default url where the packages will be pulled from.")

(defconst dk/get-package-github-content-url "https://raw.githubusercontent.com/")

(defcustom dk/get-package-branch-names '("master" "main" "release")
  "List of branch names that are acceptable to pull. The order is the order in
which they are checked if they can be used.")

(defconst dk/get-package-install-dir (concat user-emacs-directory "var/get-package/")
  "Location for all packages that were install with get-package.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; URL Builders                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dk/get-package-build-url-for-branch-list (user repo)
  "Build an url string that can be used to get a list of all branches."
  (let ((url dk/get-package-github-api-url))
    (concat url user "/" repo "/branches")))

(defun dk/get-package-build-url-for-file-list (user repo branch)
  "Build an url string that can be used to get a list of files in a repo."
  (let ((url dk/get-package-github-api-url))
    (concat url user "/" repo "/git/trees/" branch "?recursive=1")))

(defun dk/get-package-build-url-for-content (user repo branch path)
  "Build an url string that can be used to get file content from github."
  (let ((url dk/get-package-github-content-url))
    (concat url "/" user "/" repo "/" branch "/" path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Content getters                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dk/get-package-retrieve-content-and-save (url file-name)
  "Get content from url and return it as a string."
  (let ((interpreter-mode-alist nil)
	(magic-mode-alist nil)
	(magic-fallback-mode-alist nil)
	(auto-mode-alist nil))
    (with-temp-buffer
      (url-insert-file-contents url)
      (let ((direcory (file-name-directory file-name)))
	(unless (file-directory-p direcory)
	  (mkdir direcory t)))
      (write-file file-name nil))))

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

(defun dk/get-package-get-branches (url)
  "Get a list of all branches from a `repo' by `user'"
  (let* ((content (dk/get-package-retrieve-content-as-json url)))
    (mapcar (lambda (elem) (cdr (car elem))) content)))

(defun dk/get-packages-select-branch (available-branches)
  "Select a branch that will used to get the files from github."
  (let ((selectable-branches nil))
    (dolist (avail available-branches)
      (when (member avail dk/get-package-branch-names)
	(push avail selectable-branches)))
    (if (eq selectable-branches nil)
	(error "There are no fitting branches.")
      (car selectable-branches))))

(defun dk/get-packages-get-file-names (url)
  "Get all files from a repo from github."
  (let* ((content (dk/get-package-retrieve-content-as-json url))
	 (tree (nth 2 content)))
    (remove nil (mapcar (lambda (elem)
			  (let ((keyword (car (nth 0 elem)))
				(type (cdr (nth 1 elem))) 
				(file (cdr (nth 0 elem))))
			    (when (and (string-equal keyword "path")
				       (not (string-equal type "040000"))) ;; ensure not dir
			      file)))
			(cdr tree)))))

(defun dk/get-package-get-package (user repo branch)
  "Install a package with get-package."
  (let* ((branches-url (dk/get-package-build-url-for-branch-list user repo))
	 (available-branches (dk/get-package-get-branches branches-url))
	 (selected-branch (if branch branch
			    (dk/get-packages-select-branch available-branches)))
	 (files-url (dk/get-package-build-url-for-file-list user repo selected-branch))
	 (files (dk/get-packages-get-file-names files-url)))
    (dolist (file files)
      (dk/get-package-retrieve-content-and-save
       (dk/get-package-build-url-for-content user repo selected-branch file)
       (concat dk/get-package-install-dir "/" repo "/" file)))))

;; Example: (dk/get-package-get-package "Domse007" "snipsearch")

(defun dk/get-package! (&rest args)
"Get a package if git is not available. The following parameters must be
defined:

 1) :user <string>    The github user name
 2) :repo <string>    The repository
 3) :branch <string>  Optional the branch of the repo.
 4) :force <bool>     Force to install with get-package.

Example:

(dk/get-package! :user \"magnars\" :repo \"multiple-cursors.el\" :force t)

Here we are defining the recipe for multiple cursors. We force it to override
the check if it should skip because git is installed and quelpa can install it.

It is possible that :ensure set to false if get-package is used."
(declare (indent 1))
(let* ((user (plist-get args :user))
       (repo (plist-get args :repo))
       (branch (plist-get args :branch))
       (force (plist-get args :force))
       (package-path (concat dk/get-package-install-dir repo "/")))
  (when (and (or (eq (executable-find "git") nil) force)
	     (not (file-directory-p package-path)))
    (dk/get-package-get-package user repo branch))
  (when (file-directory-p package-path)
    (add-to-list 'load-path package-path))))

(provide 'base-get-package)
