(defun c4/code ()
  (c4/code-init))

(defun c4/code-init ()
  (use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

  (use-package company
    :diminish
    :hook (prog-mode . company-mode))

  (use-package flycheck
    :diminish
    :hook (prog-mode . flycheck-mode))

  (use-package lsp-mode
    :diminish
    :hook (prog-mode . lsp-deferred)
    :commands (lsp lsp-deferred))

  (use-package lsp-ui
    :after lsp-mode
    :commands lsp-ui-mode)

  (use-package lsp-ivy
    :after lsp-mode
    :commands lsp-ivy-workspace-symbol))

(provide 'code)
