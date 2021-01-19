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

(c4/user
  "Chatman R. Jr"
  "crjr.code@protonmail.com")
(c4/base)
(c4/theming 'modus-vivendi)
(c4/typography "Input" 12)
(c4/exwm)
(c4/projects "~/Workbench")
(c4/git "cr-jr")

(provide 'init)
;;;; init.el ends here
