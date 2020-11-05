;;; snipsearch.el --- Find your own snippets everywhere  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Dominik Keller <emacs@dominik-keller.ch>
;; URL: https://github.com/Domse007/.emacs.d/
;; Package-Version: 20201102.0936
;; Package-Commit: bde78180c678b233c94321394f46a81dc6dce1da
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major-mode. Use it with:
;; ┌────
;; │ (use-package snipsearch)
;; └────
;;
;; Wherever you must insert a snippet, you can find it easily. 
;; 
;; This package is in active development. 
;;
;; See the accompanying Readme.org for configuration details.
;;
;; Tags in Code:
;; - main function (#m1)
;; - display functions (#d1)
;; - Keybindings (#k1)


;;; Code:
(defvar snipsearch-mode nil "\
Non-nil if snipsearch mode is enabled.
See the `snipsearch-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `snipsearch-mode'.")

(defvar snipsearch-input-string "")

(setq snipsearch-snippet-list '(("org" "#+TITLE: " -1 "Insert the standard Org Title Heading")
				("equation" "\\[=\\]" -3 "Insert a LaTeX centerd formula")
				("frac" "\\displaystyle\\frac{}{}" -3 "Insert a LaTeX fraction"))
  ;; This list contains all the available snippets. One Snippet is a list
  ;; of the list and contains a list for all aliases, the snippet, the
  ;; cursor movement and a description of the snippet.
  ;; Form: ((String: alias_1, alias_2, alias_3, ...) String: Snippet   Int: Point   String: description)
  )



;; logic functions end here (exposed to user) (Tag: #m1)
(defun snipsearch (&optional snipsearch-search-char)
  (message snipsearch-input-string)
  (interactive)
  (switch-to-buffer "*snipsearch*")
  (snipsearch-mode)
  (snipsearch-write-to-buffer))

(defun snipsearch-snippet-at-point ()
  (interactive)
  (message "success"))

(defun snipsearch-reset-point ()
  (goto-char (+ (length snipsearch-input-string) 9)))

(defun snipsearch-search-for-results ()
  )

;; logic functions end here

;; Definitions for displaying starts here (Tag: #d1)
(defun snipsearch-write-to-buffer (&optional snipsearch-key-pressed)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert  (propertize "Search: " 'font-lock-face '(:foreground "red")) snipsearch-input-string)
    (if (string-equal snipsearch-input-string "")
      (let ((snipsearch-table-discription 1))
	(dotimes (i (- (length snipsearch-snippet-list) 1))
	   (let ((snipsearch-current-line (nth i snipsearch-snippet-list))
		 (snipsearch-length-snippet-list (length snipsearch-input-string)))
	     (when (equal snipsearch-table-discription 1)
	       (progn (insert "\nName" (make-string 16 ? ) "Snippet" (make-string 13 ? ) "Description\n"
			      (make-string 80 ?-))
		      (setq snipsearch-table-discription 0)))
	     (insert "\n" (car snipsearch-current-line)
		     (make-string (- 20 (length (car snipsearch-current-line))) ? )
		     (nth 1 snipsearch-current-line)
		     (make-string (- 20 (length (nth 1 snipsearch-current-line))) ? )
		     (nth 3 snipsearch-current-line)))))
      ;; else, when the snipsearch-input-string isn't empty
      (snipsearch-draw-table-organized)))
  (snipsearch-reset-point))

(defun snipsearch-draw-table-organized ()
  (snipsearch-search-for-results))
;; Display functions end here

;; Definitions for Keybindings start here (Tag: #k1)
(defvar snipsearch-mode-map nil "Keymap for `snipsearch-mode'")

(progn
  (setq snipsearch-mode-map (make-sparse-keymap))
  (define-key snipsearch-mode-map (kbd "a") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "a")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "b") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "b")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "c") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "c")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "d") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "d")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "e") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "e")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "f") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "f")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "g") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "g")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "h") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "h")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "i") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "i")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "j") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "j")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "k") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "k")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "l") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "l")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "m") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "m")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "n") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "n")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "o") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "o")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "p") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "p")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "q") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "q")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "r") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "r")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "s") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "s")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "t") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "t")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "u") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "u")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "v") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "v")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "w") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "w")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "x") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "x")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "y") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "y")) (snipsearch-write-to-buffer))))
  (define-key snipsearch-mode-map (kbd "z") (lambda () (interactive) (progn (setq snipsearch-input-string (concat snipsearch-input-string "z")) (snipsearch-write-to-buffer))))



  (define-key snipsearch-mode-map (kbd "RET") 'snipsearch-snippet-at-point)
  (define-key snipsearch-mode-map (kbd "DEL") (lambda () (interactive) (progn (setq snipsearch-input-string (substring snipsearch-input-string 0 -1)) (snipsearch-write-to-buffer))))
  )
;; Keybindings end here

(define-derived-mode snipsearch-mode special-mode "snipsearch"
  (use-local-map snipsearch-mode-map))

(provide 'snipsearch)
;;; beacon.el ends here
