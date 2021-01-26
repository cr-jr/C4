;;; Setup straight-el with use-package
(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
          'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package integration
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;; Need Org Mode to be first package installed because it builds the config.
(use-package org
  :straight org-plus-contrib)

  (defun c4/config-init ()
    (org-babel-tangle-file
      (concat user-emacs-directory "C4.org")
      (concat user-emacs-directory "C4.el")))

  (c4/config-init)

  ;; Setup a hook to re-tangle the config on modification.
  (add-hook 'org-mode-hook
    (lambda () (add-hook 'after-save-hook #'c4/config-init)))

(require 'cl-lib)

(require 'preamble
          (concat user-emacs-directory "modules/preamble.el"))
(require 'base
          (concat user-emacs-directory "modules/base.el"))
(require 'keybindings
          (concat user-emacs-directory "modules/keybindings.el"))
(require 'projects
          (concat user-emacs-directory "modules/projects.el"))
(require 'code
          (concat user-emacs-directory "modules/code.el"))
(require 'documents
          (concat user-emacs-directory "modules/documents.el"))
(require 'desktop
          (concat user-emacs-directory "modules/desktop.el"))

(c4/perf)
(c4/housekeeping)
(c4/debugger)

(c4/user
  :name "Chatman R. Jr"
  :email "crjr.code@protonmail.com")

(c4/base
  :theme 'minimal-light
  :typography
  (c4/typography
    :code '("Input" 13)
    :document '("Lora" 16)))

(c4/keybindings)

(c4/projects
  :path "~/Workbench"
  :username "cr-jr")

(c4/code)

(c4/org :path "~/Org")

(c4/desktop)
