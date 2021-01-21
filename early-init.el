;;;; C4 Loader
;;;;
;;;; Commentary:
;;;;
;;;; This file contains functionality that should be run before Emacs
;;;; fully starts. In this case, it preloads the module categories for use in the
;;;; init file.
;;;;
;;;; Code:

(require 'preamble
	 (concat user-emacs-directory "config/preamble.el"))
(c4/perf)
(c4/packages-setup)
(c4/housekeeping)

(require 'base
	 (concat user-emacs-directory "config/base.el"))
(require 'ui
	 (concat user-emacs-directory "config/ui.el"))
(require 'ux
	 (concat user-emacs-directory "config/ux.el"))
(require 'projects
	 (concat user-emacs-directory "config/projects.el"))
(require 'org
	 (concat user-emacs-directory "config/org.el"))
(require 'code
	 (concat user-emacs-directory "config/code.el"))
(require 'keybindings
  (concat user-emacs-directory "config/keybindings.el"))

; (require 'apps (concat user-emacs-directory "config/apps.el"))
; (require 'addons (concat user-emacs-directory "config/addons.el"))

;;;; early-init.el ends here
