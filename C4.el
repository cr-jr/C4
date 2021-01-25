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
