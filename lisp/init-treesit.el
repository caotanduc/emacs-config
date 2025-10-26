;;; -*- lexical-binding: t; -*-
;;; Tree-sitter configuration for Emacs 29–32

(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))

  ;; --- 1. Install grammars inside your Emacs config directory ---
  (setq treesit-extra-load-path
        (list (expand-file-name "treesit-grammars/" user-emacs-directory)))

  ;; Create directory if missing
  (unless (file-directory-p (car treesit-extra-load-path))
    (make-directory (car treesit-extra-load-path) t))

  ;; --- 2. Grammar sources ---
  (setq treesit-language-source-alist
        '((python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")))

  ;; --- 3. Define universal grammar readiness check ---
  (defun my/treesit-grammar-ready-p (lang)
    "Return non-nil if LANG grammar is available and usable."
    (cond
     ;; Emacs 30+ (newer API)
     ((fboundp 'treesit-language-available-p)
      (treesit-language-available-p lang))
     ;; Fallback for future changes
     ((fboundp 'treesit-language-grammar-p)
      (treesit-language-grammar-p lang))
     (t nil)))

  ;; --- 4. Auto-install grammars if missing ---
  (defun my/ensure-treesit-language-installed (lang)
    "Install tree-sitter grammar for LANG if not present."
    (unless (my/treesit-grammar-ready-p lang)
      (message "Installing missing Tree-sitter grammar for %s..." lang)
      (ignore-errors
        (treesit-install-language-grammar lang))
      (message "✅ Installed grammar for %s" lang)))

  ;; Auto-install for these languages
  (dolist (lang '(python))
    (my/ensure-treesit-language-installed lang))

  ;; --- 5. Prefer Tree-sitter major modes ---
  (setq major-mode-remap-alist
        '((python-mode . python-ts-mode)))


  ;; --- 6. Better syntax highlighting ---
  (setq treesit-font-lock-level 4))

(provide 'init-treesit)
