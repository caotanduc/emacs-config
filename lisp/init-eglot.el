;;; init-eglot.el --- Eglot setup for Python -*- lexical-binding: t; -*-

(require 'eglot)

(use-package company
  :config
  (setq company-idle-delay 0.1
	company-minimum-prefix-length 1))

(use-package python-ts-mode
  :hook ((python-ts-mode . eglot-ensure)
	 (python-ts-mode . company-mode))
  :mode (("\\.py\\'" . python-ts-mode)))

;; Optional: customize Eglot behavior
(with-eval-after-load 'eglot
  ;; Tell Eglot where to find language servers if needed
  (add-to-list 'eglot-server-programs
	       '(python-ts-mode .("ruff" "server" :initializationOptions (:preferences () :hints (:parameterName t :callArgumentNames "all" :functionReturnTypes t)))))

  (add-hook 'after-save-hook 'eglot-format))

(provide 'init-eglot)
;;; init-eglot.el ends here
