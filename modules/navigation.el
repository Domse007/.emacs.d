;;; emacs.el - This is a configuration to make adjustments to emacs that aren't packages.

;; commentary:
;; - The code doesen't depend on any additional binaries.
;; - This is mostly copied from https://www.github.com/jakobklemm/emacs

;; An Ivy-enhanced alternative to isearch.
(use-package counsel
  :ensure t
  :diminish ivy-mode counsel-mode
  :defines
  (projectile-completion-system magit-completing-read-function)
  :bind
  (("C-s" . swiper)
   ("M-s r" . ivy-resume)
   ("C-c v p" . ivy-push-view)
   ("C-c v o" . ivy-pop-view)
   ("C-c v ." . ivy-switch-view)
   ("M-s c" . counsel-ag)
   ("M-o f" . counsel-fzf)
   ("M-o r" . counsel-recentf)
   ("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("C-w" . ivy-backward-kill-word)
   ("C-k" . ivy-kill-line)
   ("C-j" . ivy-immediate-done)
   ("RET" . ivy-alt-done)
   ("C-h" . ivy-backward-delete-char))
  :preface
    (defun ivy-format-function-pretty (cands)
      "Transform CANDS into a string for minibuffer."
      (ivy--format-function-generic
       (lambda (str)
         (concat
             (all-the-icons-faicon "hand-o-right" :height .85 :v-adjust .05 :face 'font-lock-constant-face)
             (ivy--add-face str 'ivy-current-match)))
       (lambda (str)
         (concat "  " str))
       cands
       "\n"))
    :hook
    (after-init . ivy-mode)
    (ivy-mode . counsel-mode)
    :custom
    (counsel-yank-pop-height 15)
    (enable-recursive-minibuffers t)
    (ivy-use-selectable-prompt t)
    (ivy-use-virtual-buffers t)
    (ivy-on-del-error-function nil)
    (swiper-action-recenter t)
    (counsel-grep-base-command "ag -S --noheading --nocolor --nofilename --numbers '%s' %s"))

;; A generic completion mechanism for Emacs.
(use-package swiper
  :ensure t)

;; A generic completion mechanism for Emacs.
(use-package ivy
  :ensure t
  :init (ivy-mode 1)
  :config (setq ivy-use-virtual-buffers t
		enable-recursive-minibuffers t
		search-default-mode #'char-fold-to-regexp
		ivy-posframe-height-alist '((swiper . 20)
					    (t      . 15))
		ivy-re-builders-alist '((t . orderless-ivy-re-builder))) ;; enable orderless
  :bind ("C-x C-b" . ivy-switch-buffer))

;; An alternative interface for M-x in Emacs.
(use-package amx
  :ensure t
  :after ivy
  :custom
  (amx-backend 'auto)
  (amx-save-file "~/.emacs.d/var/amx-items")
  (amx-history-length 50)
  (amx-show-key-bindings nil)
  :config (amx-mode 1))

;; Display icons for all buffers in ivy.
(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

;; A more friendly interface for ivy
(use-package ivy-rich
  :if window-system
  :ensure t
  :after ivy
  :custom (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  :config (ivy-rich-mode 1))

;; Fuzzy matching for Emacs
(use-package flx
  :ensure t)

;; Let ivy use posframe to show its candidate menu.
(use-package ivy-posframe
  :ensure t
  :if window-system
  :after ivy
  :init (ivy-posframe-mode 1)
  (setq ivy-posframe-parameters
	'((left-fringe . 4)
	  (right-fringe . 4)))
  :config (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center))
		ivy-posframe-border-width 4)
  :custom-face
  ;; (ivy-posframe ((t (:background "#0C0D0D"))))
  ;; (ivy-posframe-cursor ((t (:background "#50C517"))))  
  (ivy-posframe-border ((t (:background "#FD971F")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * THEME VARIABLES:			        ;;
;; ** doom-vibrant			        ;;
;; user-ivy-posframe-background-color "#171717" ;;
;; user-ivy-posframe-border-color "#3B2441"     ;;
;; user-ivy-posframe-cursor-color "#51AFEF"     ;;
;; 					        ;;
;; ** humanoid-dark			        ;;
;; user-ivy-posframe-background-color "#0C0D0D" ;;
;; user-ivy-posframe-border-color "#FF9505"     ;;
;; user-ivy-posframe-cursor-color "#50C517"     ;;
;;                                              ;;
;; ** doom-monokai-classic		        ;;
;; user-ivy-posframe-border-color "#F92660"     ;;
;;                                              ;;
;; ** doom-molokai          		        ;;
;; user-ivy-posframe-border-color "#FD971F"     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable any mouse input in org-mode.
(use-package disable-mouse
  :ensure t
  :hook (org-mode . disable-mouse-mode))

;; enhance the completion framework
(use-package orderless
  :ensure t
  :init (icomplete-mode)
  :config (completion--styles '(orderless)))

(provide 'navigation.el)
;; navigation.el
