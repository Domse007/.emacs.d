;;; init.el --- -*- lexical-binding: t -*-
;; DeferGC
(setq gc-cons-threshold 100000000)
;; -DeferGC

;; UnsetPES
(setq package-enable-at-startup nil)
;; -UnsetPES

;; UnsetFNHA
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
;; -UnsetFNHA

;; UnsetSRF
(setq site-run-file nil)
;; -UnsetSRF

;; DisableUnnecessaryInterface
(menu-bar-mode -1)
(unless (display-graphic-p)
  (setq default-frame-alist '((fullscreen . maximized)
			      (menu-bar-lines . 0)
			      (tool-bar-lines . 0)
			      (vertical-scroll-bars . nil))
	initial-frame-alist '((fullscreen . maximized)
			      (menu-bar-lines . 0)
			      (tool-bar-lines . 0)
			      (vertical-scroll-bars . nil))))
;; -DisableUnnecessaryInterface

(provide 'early-init)
