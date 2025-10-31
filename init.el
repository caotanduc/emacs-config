(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)
(setq visible-bell t)

(add-to-list 'default-frame-alist `(font . "Iosevka Extended-20"))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Remove title bar / window decorations
(add-to-list 'initial-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(undecorated . t))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; editor config
(electric-pair-mode t)

(setq tab-always-indent 'complete)
(setq eshell-destroy-buffer-when-process-dies t)

(setq custom-file (expand-file-name ".emacs.custom.el" user-emacs-directory))
(load custom-file)

(let ((data-dir (expand-file-name "tmp/" user-emacs-directory)))
  (make-directory data-dir t)
  (setq backup-directory-alist `(("." . ,(expand-file-name "backups/" data-dir))))
  (setq auto-save-list-file-prefix (expand-file-name "auto-saves/sessions/" data-dir))
  (setq auto-save-file-name-transforms `((".*", (expand-file-name "auto-saves" data-dir) t))))

(let ((data-dir (expand-file-name "var/" user-emacs-directory)))
  (make-directory data-dir t)
  (setq auto-save-list-file-prefix (expand-file-name "auto-save/" data-dir))
  (setq eshell-directory-name (expand-file-name "eshell/" data-dir))
  (setq transient-history-file (expand-file-name "transient/history.el" data-dir))
  (setq package-user-dir (expand-file-name "elpa/" data-dir)))

;; Packages
(setq use-package-always-defer t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Enable Vertico.
(use-package vertico
  :custom
  (vertico-resize t)
  (vertico-cycle t)
  :init
  (vertico-mode)
  (vertico-insert))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Allow nested minibuffers
(setq enable-recursive-minibuffers t)

;; Hide commands in M-x that don’t work in current mode
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Make minibuffer prompt read-only and non-editable
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

(use-package marginalia
  :init
  (marginalia-mode))

;; Example configuration for Consult
(use-package consult
  :disabled t
  :bind (;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Eshell
         :map eshell-mode-map
         ("s-r" . consult-history)
         ;; Minibuffer 
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
)

(use-package magit)

(use-package gruvbox-theme)

(use-package vterm)

(use-package multiple-cursors)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; macos configuration for emacs terminal
(unless window-system
  (when (and (executable-find "pbcopy") (executable-find "pbpaste"))
    (defun copy-from-osx ()
      (shell-command-to-string "pbpaste"))
    (defun paste-to-osx (text &optional push)
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))))
    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx)))

(when window-system
  (load-theme 'gruvbox t)
  (setq exec-path (append exec-path '("/opt/homebrew/bin/"
                                      "~/.cargo/bin/"
                                      "/usr/local/bin/")))
)

(use-package breadcrumb)

(use-package company
  :init
  (global-company-mode))

(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")))

(use-package eglot
  :hook ((eglot--managed-mode . breadcrumb-local-mode)
         (eglot--managed-mode . eglot-booster-mode))
  :bind (:map eglot-mode-map ("<f2>" . eglot-rename))
  :config
  ;; (fset #'jsonrpc--log-event #'ignore)
  ;; (setq jsonrpc-event-hook nil)
  ;; Run both basedpyright and ruff for python-ts-mode
  (add-to-list 'eglot-server-programs
               `(python-ts-mode . ,(eglot-alternatives '(("pyright-langserver" "--stdio")
                                                         ("basedpyright-langserver" "--stdio")
                                                         ("ruff" "server")))))

  ;; Configure basedpyright and inlay hints
  (setq-default eglot-workspace-configuration
                '(:basedpyright
                  (:typeCheckingMode "recommended"
				     :analysis
				     (:diagnosticSeverityOverrides
				      (:reportUnusedCallResult "none")))
                  :inlayHints
                  (:callArgumentNames "all"
				      :functionReturnTypes t)
		  :pyright ()
		  :ruff ())))

(use-package reformatter
  :config
  (reformatter-define ruff-check
    :program "ruff"
    :args `("check" "--fix" "--stdin-filename" ,buffer-file-name "-"))
  (reformatter-define ruff-organize-imports
    :program "ruff"
    :args `("check" "--select" "I" "--fix" "--stdin-filename" ,buffer-file-name "-"))
  (reformatter-define ruff-format
    :program "ruff"
    :args `("format" "--stdin-filename" ,buffer-file-name "-")))

(use-package python-ts-mode
  :ensure nil
  :hook ((python-ts-mode . eglot-ensure)
         (python-ts-mode . conda-env-autoactivate-mode)
         (python-ts-mode . ruff-check-on-save-mode)
         (python-ts-mode . ruff-organize-imports-on-save-mode)
         (python-ts-mode . ruff-format-on-save-mode))
  :mode (("\\.py\\'" . python-ts-mode))
  :config
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq electric-indent-inhibit nil)
  (electric-indent-mode 1))

(use-package conda
  :config
  (conda-env-initialize-eshell)
  (conda-env-initialize-interactive-shells)
  (conda-env-autoactivate-mode t)
  (add-hook 'find-file-hook (lambda () (when (bound-and-true-p conda-project-env-path)
                                         (conda-env-activate-for-buffer))))
  (custom-set-variables
   '(conda-anaconda-home "~/miniconda3/"))

  (setq conda-env-home-directory (expand-file-name "~/miniconda3/")))

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(require 'eglot-booster)
(require 'vscode)
(require 'dashboard)

(global-vscode-mode 1)
(add-hook 'emacs-startup-hook #'my/welcome-buffer)
