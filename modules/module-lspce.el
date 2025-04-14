(use-package lspce
  :quelpa (lspce :fetcher github :repo "zbelial/lspce" :files ("*"))
  :init
  (let ((default-directory (file-name-directory (locate-library "lspce"))))
    (message "Entering %s" (pwd))
    (unless (or (file-exists-p "lspce-module.so")
		(file-exists-p "lspce-module.d")
                (file-exists-p "lspce-module.dll"))
      (shell-command "cargo build --release")
      (cond ((eq system-type 'gnu/linux)
	     (progn (copy-file "target/release/liblspce_module.so"
			       "lspce-module.so" t)
		    (copy-file "target/release/liblspce_module.d"
			       "lspce-module.d" t)))
	    ((eq system-type 'windows-nt)
	     (progn (copy-file "target/release/lspce_module.dll"
			       "lspce-module.dll" t)
		    (copy-file "target/release/lspce_module.d"
			       "lspce-module.d" t))))
      (message "Done.")))
  (require 'lspce)
  :hook
  ((rust-mode . lspce-mode)
   (python-mode . lspce-mode)))

(use-package eldoc-box
  :hook
  ((window-size-change-functions . dk/eldoc-set-box-sizes))
  :init
  (defun dk/approximate-frame-width ()
    (* (* (frame-width) (frame-char-width)) 2))
  (defun dk/approximate-frame-height ()
    (* (* (frame-height) (frame-char-height)) 2))
  (defun dk/eldoc-get-size-advice ()
    (let ((width (dk/approximate-frame-width))
	  (height (dk/approximate-frame-height)))
      (cons (min (/ width 5) 1000)
	    (min (/ height 4) 1000))))
  ;; TODO: broken, mb due to prerelease emacs version? Error:
  ;; Error muted by safe_call: (dk/eldoc-set-box-sizes #<frame EMACS - 31.0.50 0x5630d55009e8>)
  ;; signaled (wrong-number-of-arguments #[nil ((let ((w-h (dk/eldoc-get-size-advice)))
  ;; (progn (setq eldoc-box-max-pixel-width (car w-h)) (setq eldoc-box-max-pixel-height (cdr w-h))))) nil] 1)
  ;; This is not even the actual code. Maybe some old compiled version of function?
  (defun dk/eldoc-set-box-sizes ()
    (let ((w-h (dk/eldoc-get-size-advice)))
      (setq eldoc-box-max-pixel-width (car w-h)
	    eldoc-box-max-pixel-height (cdr w-h))))
  ;; set at initialisation
  (dk/eldoc-set-box-sizes)
  (advice-add #'eldoc-box--window-side :around
	      (lambda (orig-fun &rest args)
		(if (eq (treemacs-current-visibility) 'visible)
		    ;; left is broken in my opinion, but what do I know
		    'left (apply orig-fun args))))
  :hook
  ((lspce-mode . eldoc-box-hover-mode)))

(provide 'module-lspce)
