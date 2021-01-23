;;;; C4 Module Loader
;;;;
;;;; Commentary:
;;;;
;;;; The early-init file loads all the modules available so they can be called
;;;; for execution seamlessly in the init.el itself.
;;;;
;;;; Code:

(require 'preamble
	 (concat user-emacs-directory "config/preamble.el"))
(require 'base
	 (concat user-emacs-directory "config/base.el"))
(require 'keybindings
  (concat user-emacs-directory "config/keybindings.el"))
(require 'projects
	 (concat user-emacs-directory "config/projects.el"))
(require 'code
	 (concat user-emacs-directory "config/code.el"))
(require 'org-setup
	 (concat user-emacs-directory "config/org-setup.el"))
(require 'ui
  (concat user-emacs-directory "config/ui.el"))
(require 'ux
  (concat user-emacs-directory "config/ux.el"))
