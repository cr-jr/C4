;;; C4 --- CRJr's Coding Command Center
;;;
;;; Commentary:
;;;
;;; This is my personal Emacs configuration for day to day work.
;;; In most cases, it shouldn't be necessary to change this file.
;;; If you want to modify this configuration, the place to start
;;; is C4.org.
;;;
;;; Code:

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

(load-file (concat user-emacs-directory "C4.el"))

;; Setup a hook to re-tangle the config on modification.
(add-hook 'org-mode-hook
  (lambda () (add-hook 'after-save-hook #'c4/config-init)))

(provide 'init)
;;; init.el ends here
