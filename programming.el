;;; 
;;; Jacob Anduril Emacs Programming.el
;;;

(use-package nix-mode)

(use-package haskell-mode)

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none)
  :diminish (auto-revert-mode)
  :diminish (lsp-lens-mode)
  :commands (lsp lsp-deferred)
  :defer (direnv-mode)
  :init
  (setq lsp-keymap-prefix "C-c l")
  ;; Not sure what this does
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex)))
  (lsp-modeline-code-actions-mode nil)
  :hook (haskell-mode . lsp-deferred)
  :hook (lsp-completion-mode . my/lsp-mode-setup-completion)
  :commands lsp)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil))

(use-package lsp-haskell
  :defer (direnv-mode)
  :config
  (add-hook 'haskell-mode-hook #'lsp)
  (setq lsp-haskell-server-path "haskell-language-server"
		lsp-haskell-formatting-provider "fourmolu"
		lsp-haskell-plugin-stan-global-on nil))

(use-package flycheck
  :hook (flycheck-mode . flycheck-set-indication-mode)
  :config
  (setq-default flycheck-indication-mode 'right-margin))

(use-package git-gutter :diminish git-gutter-mode :init (global-git-gutter-mode))

;; Default package, diminish
(use-package eldoc :diminish eldoc-mode)

(use-package direnv
  :config
  (direnv-mode))
