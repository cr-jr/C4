;;;; C4 Loader
;;;;
;;;; Commentary:
;;;;
;;;; This file contains functionality that should be run before Emacs
;;;; fully starts. In this case, it preloads the module categories for use in the
;;;; init file.
;;;;
;;;; Code:

(require 'base
  (concat user-emacs-directory "config/base.el"))
(c4/init)

(require 'ui
  (concat user-emacs-directory "config/ui.el"))
(require 'ux
  (concat user-emacs-directory "config/ux.el"))
(require 'projects (concat user-emacs-directory "config/projects.el"))
; (require 'code (concat user-emacs-directory "config/code.el"))
; (require 'org (concat user-emacs-directory "config/org.el"))
; (require 'apps (concat user-emacs-directory "config/apps.el"))
; (require 'addons (concat user-emacs-directory "config/addons.el"))
