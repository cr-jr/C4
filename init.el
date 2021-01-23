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

(c4/perf)
(c4/packages-setup)
(c4/housekeeping)

(c4/user
  :name "Chatman R. Jr"
  :email "crjr.code@protonmail.com")

(c4/base
  :theme 'minimal-light
  :typography
  (c4/typography
    :fixed '("Input" 12)
    :variable '("Lora" 16)))

(c4/keybindings)

(c4/projects
  :path "~/Workbench"
  :username "cr-jr")

(c4/code)

(c4/org
  :path "~/Org"
  :theme
  (c4/org-theme
    :body "Lora"
    :headings "Lato"
    :code "Input"
    :base 12))

(c4/exwm)

(provide 'init)
;;;; init.el ends here
