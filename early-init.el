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
  (setq default-frame-alist '((fullscreen . maximized)
			      (menu-bar-lines . 0)
			      (tool-bar-lines . 0)
			      (vertical-scroll-bars . nil))
	initial-frame-alist '((fullscreen . maximized)
			      (menu-bar-lines . 0)
			      (tool-bar-lines . 0)
			      (vertical-scroll-bars . nil))))
;; -DisableUnnecessaryInterface

;; (set-face-attribute 'default nil
;;                     :height 90)
;; (set-face-attribute 'fixed-pitch nil
;;                     :height 90)

(provide 'early-init)
