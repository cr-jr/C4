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

;;; Package management

;; Setup straight.el with use-package
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

;;; Override built-ins with newer packages

;; Needed for eglot
(straight-use-package 'project)
(straight-use-package 'xref)
(straight-use-package 'eldoc)
(straight-use-package 'flymake)

;; Needed for Org
(straight-use-package 'org-plus-contrib)

;;; Load in config
(org-babel-load-file
  (expand-file-name "C4.org" user-emacs-directory))

(provide 'init)
;;; init.el ends here
