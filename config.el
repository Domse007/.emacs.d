;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(setq doom-theme 'doom-bluloco-dark)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq evil-vsplit-window-right t
      evil-split-window-below t
      evil-move-beyond-eol t)

(defun dk/org-roam-get-template (file)
  "Get the string of a template file and modify it accordingly. Errors when
FILE does not exist."
  (if (file-exists-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(replace-string "[NAME]" user-full-name)
	(buffer-string))
    (error "File %s does not exist." file)))

(defconst dk/org-roam-default-template
  (dk/org-roam-get-template (expand-file-name "templates/default.org"
					      doom-user-dir))
  "Template string of the default org-roam template.")

(defconst dk/org-roam-program-template
  (dk/org-roam-get-template (expand-file-name "templates/program.org"
					      doom-user-dir))
  "Tempate string for describing the functionalities of a program.")

(use-package! org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  ((org-roam-directory "~/SynologyDrive/Notes/")
   (org-roam-completion-everywhere t)
   (org-roam-db-location
    (expand-file-name "org/org-roam.db" doom-cache-dir))
   (org-roam-capture-templates
    `(("d" "default" plain "%?" :target
       (file+head "%<%Y%m%d>-${slug}.org" ,dk/org-roam-default-template)
       :unnarrowed t)
      ("p" "program" plain "%?" :target
       (file+head "program/%<%Y%m%d>-${slug}.org" ,dk/org-roam-program-template)
       :unnarrowed t))))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert))
  :config
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
		 (display-buffer-in-side-window)
		 (side . right)
		 (slot . 0)
		 (window-width . 0.25)
		 (preserve-size . (t nil))
		 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  (org-roam-db-autosync-enable))

(use-package! org-roam-ui
  :after org-roam
  :custom
  ((org-roam-ui-sync-theme t)
   (org-roam-ui-follow nil)
   (org-roam-ui-update-on-save t)
   (org-roam-ui-open-on-start nil)))

(use-package! org-roam-timestamps
  :config
  (setq org-roam-timestamps-remember-timestamps t)
  (setq org-roam-timestamps-minimum-gap 3600))

(use-package! org
  :hook
  ((org-mode-hook . org-toggle-pretty-entities)
   (org-mode-hook . prettify-symbols-mode)
   (org-mode-hook . (lambda () (setq fill-column 70)))
   (org-mode  . turn-on-auto-fill)
   (org-mode . company-mode))
  :custom
  ((org-src-fontify-natively t)
   (org-highlight-latex-and-related '(latex script entities))
   (org-startup-with-latex-preview t)
   (org-adapt-indentation nil)
   (org-src-tab-acts-natively t)
   (org-catch-invisible-edits 'smart)
   (org-ctrl-k-protect-subtree t)
   (prettify-symbols-unprettify-at-point 'right-edge)
   (org-latex-preview-ltxpng-directory "~/.ltxpng/")
   ;; (org-latex-listings 'minted)
   (org-latex-src-block-backend 'engraved)
   (org-latex-packages-alist '(("AUTO" "babel"  nil nil)
			       (""     "mhchem" t   nil)))
   (org-return-follows-link t)
   (org-confirm-babel-evaluate nil)
   (org-edit-src-content-indentation 0)
   (org-src-preserve-indentation t)
   (org-export-babel-evaluate t)
   (org-id-locations-file
    (concat doom-user-dir "org/.org-id-locations"))
   (org-export-allow-bind-keywords t)
   (org-image-actual-width nil)
   (org-special-ctrl-a/e t)))

;; For setting org-latex-src-block-backend
(use-package! engrave-faces)

(use-package! htmlize
  :defer t)

(use-package! org-fragtog
  :defer t
  :if (window-system)
  :hook
  ((org-mode . org-fragtog-mode)))

(use-package! org-appear
  :defer t
  :if (window-system)
  :hook
  (org-mode . org-appear-mode)
  :custom
  ((org-hide-emphasis-markers t)
   (org-appear-autoemphasis t)
   (org-appear-autolinks t)
   (org-appear-autosubmarkers t)))

(use-package! org-tempo
  :ensure nil)

(use-package! org-modern
  :hook
  ((org-mode . org-modern-mode)))
