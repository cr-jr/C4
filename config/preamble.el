;;;; C4 Preamble
;;;;
;;;; Commentary:
;;;;
;;;; This file contains modules for the pre-init tasks and modules of my config
;;;; including performance optimizations and housekeeping tasks.
;;;;
;;;; Code:

(defun c4/perf ()
  "Handles performance optimizations"

  ;; Raise the garbage collection threshold high as emacs starts
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))

  ;; Drop it down once loaded
  (add-hook
  'after-init-hook
  #'(lambda () (setq gc-cons-threshold 80000))))

(defun c4/packages-setup ()
  "Initializes package management with straight.el & use-package."
  ;; Initialize straight.el for package management
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
  (setq straight-use-package-by-default t))

(defun c4/housekeeping ()
  "A helper for should-be-defaults"
  ;; Lockfiles do more harm than good
  (setq create-lockfiles nil)

  ;; Custom files just add clutter
  (setq custom-file null-device)

  (add-hook 'before-save-hook
    'delete-trailing-whitespace)    ; Delete trailing whitespace on save

  ;; Create parent dirs when opening new files
  (add-to-list 'find-file-not-found-functions #'c4/create-parent-on-file-find)

  ;; I really don't like clutter. Really :P
  (use-package no-littering
  :custom
  (auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))))

(defun c4/create-parent-on-file-find ()
  "Ensures that the parent dirs are created for a nonexistent file."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
	       (y-or-n-p (format
			  "Directory `%s' does not exist! Create it?"
			  parent-directory)))
      (make-directory parent-directory t))))

(provide 'preamble)
;;;; preamble.el ends here
