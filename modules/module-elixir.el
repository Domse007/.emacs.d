(defvar elixir-ls-download-url
  "https://github.com/elixir-lsp/elixir-ls/releases/download/v0.30.0/elixir-ls-v0.30.0.zip"
  "Direct download URL for the ElixirLS release zip.
Update this string to pin a different version.")

(defvar elixir-ls-install-dir
  (expand-file-name "var/elixir-ls" user-emacs-directory)
  "Directory where ElixirLS will be installed (~/.emacs.d/elixir-ls/ by default).")

(defun elixir-ls-server-script ()
  "Return the path to the installed language_server.sh (or .bat on Windows)."
  (expand-file-name (if (eq system-type 'windows-nt)
                        "language_server.bat"
                      "language_server.sh")
                    elixir-ls-install-dir))

;;;###autoload
(defun elixir-ls-install ()
  "Download and install ElixirLS for use with lspce.
Extracts into `elixir-ls-install-dir' and makes the server script executable."
  (interactive)
  (when (and (not (eq system-type 'windows-nt))
             (not (executable-find "unzip")))
    (error "Cannot find 'unzip' in PATH; please install it first"))
  (let ((zip-file (expand-file-name "elixir-ls.zip" temporary-file-directory)))
    (message "Downloading ElixirLS...")
    (url-copy-file elixir-ls-download-url zip-file t)
    (message "Extracting to %s ..." elixir-ls-install-dir)
    (make-directory elixir-ls-install-dir t)
    (let ((exit-code
           (if (eq system-type 'windows-nt)
               (call-process "powershell" nil nil nil
                             "-NoProfile" "-Command"
                             (format "Expand-Archive -Force '%s' '%s'"
                                     zip-file elixir-ls-install-dir))
             (call-process "unzip" nil nil nil "-o" zip-file "-d" elixir-ls-install-dir))))
      (delete-file zip-file)
      (unless (= exit-code 0)
        (error "Extraction failed (exit code %d)" exit-code)))
    (unless (eq system-type 'windows-nt)
      (set-file-modes (elixir-ls-server-script) #o755))
    (message "ElixirLS installed. Add to your init.el:\n(add-to-list 'lspce-server-programs '(\"elixir\" \"%s\" \"\"))"
             (elixir-ls-server-script))))

(use-package elixir-mode)

(provide 'module-elixir)
