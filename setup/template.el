(setq dk/user-file-version {VERSION})
(setq user-full-name "{NAME}")
(setq user-mail-address "{EMAIL}")
(setq dk/user-system-base-path "~/")
(setq dk/org-roam-dir "~/Notes/")
(setq dk/use-40-percent-keyboard {KEYBOARD})
(setq dk/get-package-override-git-availability nil)

(defun dk/user-file-setup ()
  (use-modules! '(
                  custom-search
                  custom-theme
                  custom-helm
                  ;; custom-ivy
                  text-org-mode
                  text-org-spell
                  text-org-roam
                  programming-base
                  programming-rust
                  programming-elisp
                  programming-python
                  programming-haskell
                  optional-visuals
                  ))

  (dk/theme! 'humanoid-dark))

(defun dk/user-file-custom ())

