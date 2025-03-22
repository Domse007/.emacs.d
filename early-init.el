;;; early-init.el --- -*- lexical-binding: t -*-

;; DeferGC
(defvar dk/original-gc-threshold gc-cons-threshold
  "Save the original `gc-cons-threshold'.")

(setq gc-cons-threshold (* 1024 1024 1024))
;; -DeferGC

;; UnsetPES
(setq package-enable-at-startup nil)
;; -UnsetPES

;; UnsetFNHA
;; (defvar file-name-handler-alist-original file-name-handler-alist)
;; (setq file-name-handler-alist nil)
;; -UnsetFNHA

;; UnsetSRF
(setq site-run-file nil)
;; -UnsetSRF

;; DisableUnnecessaryInterface
(menu-bar-mode -1)
(unless (display-graphic-p)
  (setq default-frame-alist '(;; (fullscreen . maximized)
			      (background-color . "#000000")
			      (menu-bar-lines . 0)
			      (tool-bar-lines . 0)
			      (vertical-scroll-bars . nil))
	initial-frame-alist '(;; (fullscreen . maximized)
			      (menu-bar-lines . 0)
			      (tool-bar-lines . 0)
			      (vertical-scroll-bars . nil))))
;; -DisableUnnecessaryInterface

(defvar dk/original-frame-decorations default-frame-alist)

(defun dk/toggle-frame-decorations ()
  (interactive)
  (if (assoc 'undecorated default-frame-alist)
      (setq default-frame-alist '((undecorated . t)))
    (setq default-frame-alist dk/original-frame-decorations)))

(provide 'early-init)
