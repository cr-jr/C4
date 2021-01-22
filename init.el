;;;; C4 (CRJr's Code Command Center) -*- lexical-binding: t; -*-
;;;;
;;;; Copyright (C) 2021 Chatman R. Jr
;;;;
;;;; Author: Chatman R. Jr <http://github/cr-jr>
;;;; Maintainer: Chatman R. Jr <crjr.code@protonmail.com>
;;;; Created: January 09, 2021
;;;; Modified: January 09, 2021
;;;; Version: 0.0.1
;;;; Keywords:
;;;; Homepage: https://github.com/cr-jr/c4
;;;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;;;
;;;; This file is not part of GNU Emacs.
;;;;
;;;; Commentary:
;;;;
;;;; This config is built for my writing and coding needs. It's
;;;; based off code from the Emacs from Scratch tutorial series,
;;;; uses some Doom Emacs/Spacemacs keybinding mnemonics (so I can
;;;; save my fingers), and some Sanemacs defaults.
;;;;
;;;; Other than that, it's for my own study of Emacs Lisp as well as
;;;; learning how to use it in the ways that are most effective for /me/.
;;;;
;;;; Code:

(require 'preamble
	 (concat user-emacs-directory "config/preamble.el"))
(c4/perf)
(c4/packages-setup)
(c4/housekeeping)

(require 'base
	 (concat user-emacs-directory "config/base.el"))
(c4/user
  "Chatman R. Jr"
  "crjr.code@protonmail.com")
(c4/ui)
(c4/theming 'minimal-light)
(c4/typography "Input" 12)
(c4/ux)

(require 'keybindings
  (concat user-emacs-directory "config/keybindings.el"))
(c4/keybindings)

(require 'projects
	 (concat user-emacs-directory "config/projects.el"))
(c4/projects "~/Workbench")
(c4/git "cr-jr")

(require 'code
	 (concat user-emacs-directory "config/code.el"))
(c4/code)

(require 'org-setup
	 (concat user-emacs-directory "config/org-setup.el"))
(c4/org
  :path "~/Org"
  :theme
  (c4/org-theme
    :body "Lora"
    :headings "Lato"
    :code "Input"
    :base 12))

(require 'ui
  (concat user-emacs-directory "config/ui.el"))
(c4/exwm)

(provide 'init)
;;;; init.el ends here
