;;; Setup straight.el with use-package
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

;;; Raise the garbage collection threshold high as emacs starts
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;;; Drop it down once loaded
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 1000000)))

;;; Lockfiles do more harm than good
(setq create-lockfiles nil)

;;; Custom files just add clutter
(setq custom-file null-device)

;;; Put temporary and data files in proper locations
(use-package no-littering
  :custom
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;;; Create parent dirs when opening new files
(add-to-list 'find-file-not-found-functions #'C4/create-parent)

(defun C4/create-parent ()
  "Ensures that the parent dirs are created for a nonexistent file."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format
                          "Directory `%s' does not exist! Create it?"
                          parent-directory)))
      (make-directory parent-directory t))))

;;; Clean up whitespace in all major modes on save
(use-package whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode t))

;;; Setup which-key for keybinding discoverability
(use-package which-key
  :custom
  (which-key-idle-delay 1.5)
  (which-key-enable-extended-define-key t)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  :config
  (which-key-mode))

;;; Command mode initialization
(use-package ryo-modal
  :commands ryo-modal-mode
  :bind
  ("C-SPC" . ryo-modal-mode)
  ("<menu>" . ryo-modal-mode)
  :hook
  (text-mode . ryo-modal-mode)
  (prog-mode . ryo-modal-mode)
  (exwm-mode . ryo-modal-mode)
  :config
  ;; which-key integration
  (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist)
  ;; C-i needs to be its own keybinding
  (keyboard-translate ?\C-i ?\M-i))

;;; Setup transient mode-ish interfaces
(use-package hydra)

;;; Actions: insertion
(ryo-modal-keys
 ("q" ryo-modal-mode :name "insert at point")
 ("SPC SPC" ryo-modal-mode :name "insert at point"))

;;; Action modifiers
(ryo-modal-keys
 ;; procedural modifier
 ("." ryo-modal-repeat)
 ;; numeric modifiers
 ("-" "M--" :norepeat t)
 ("0" "M-0" :norepeat t)
 ("1" "M-1" :norepeat t)
 ("2" "M-2" :norepeat t)
 ("3" "M-3" :norepeat t)
 ("4" "M-4" :norepeat t)
 ("5" "M-5" :norepeat t)
 ("6" "M-6" :norepeat t)
 ("7" "M-7" :norepeat t)
 ("8" "M-8" :norepeat t)
 ("9" "M-9" :norepeat t))

;;; Actions: movement
(ryo-modal-keys
 ("i" previous-logical-line :name "previous line")
 ("I" scroll-down-command :name "scroll up the buffer")
 ("M-i" beginning-of-buffer :name "jump point to beginning of buffer")
 ("k" next-logical-line :name "next line")
 ("K" scroll-up-command :name "scroll down the buffer")
 ("C-k" end-of-buffer :name "jump point to end of buffer")
 ("j" backward-char :name "previous char")
 ("J" backward-word :name "jump point to previous word")
 ("C-j" beginning-of-line-text :name "jump point to beginning text of line")
 ("M-j" beginning-of-line :name "jump point to beginning of line")
 ("l" forward-char :name "next char")
 ("L" forward-word :name "jump point to next word")
 ("C-l" end-of-line :name "jump point to end of line")
 ("M-l" end-of-line :name "jump point to end of line"))

(defun C4/mark-line ()
  "Mark the entire line"
  (interactive)
  (end-of-line)
  (set-mark-command nil)
  (beginning-of-line))

;;; Actions: marking/selecting text
(ryo-modal-keys
 ("m" set-mark-command :name "set a mark at point")
 ("M"
  (("w" mark-word :name "mark word")
   ("l" C4/mark-line :name "mark current line")
   ("p" mark-paragraph :name "mark paragraph")) :name "semantic mark"))

;;; Actions: killing/cutting text
(ryo-modal-keys
 ("x" kill-region :wk "cut selection")
 ("X" clipboard-kill-region :wk "cut selection (system)"))

;;; Actions: copy/paste
(ryo-modal-keys
 ("c" kill-ring-save :name "copy selection")
 ("C" clipboard-kill-ring-save :name "copy selection (system)")
 ("v" yank :name "paste")
 ("V" clipboard-yank :name "paste (system)"))

;;; Actions: deleting text
(ryo-modal-keys
 ("d" delete-char :wk "delete char after point")
 ("D"
  (("d" backward-delete-char :name "delete char before point")
   ("r" delete-region :name "delete-region"))))

;;; Command modifiers
(ryo-modal-keys
 ("SPC u" universal-argument :name "command modifier"))

;;; Domain: buffers
(ryo-modal-keys
 ;; state
 ("SPC b"
  (("d" kill-this-buffer :name "kill")
   ("D" kill-some-buffers :name "kill multiple")
   ("k" kill-this-buffer :name "kill")
   ("K" kill-some-buffers :name "kill multiple")
   ("w" save-buffer :name "save")
   ("W" save-some-buffers :name "save modified")
   ;; narrowing
   ("n"
    (("n" widen :name "reset")
     ("d" narrow-to-defun :name "to defun")
     ("p" narrow-to-page :name "to page")
     ("r" narrow-to-region :name "to region")) :name "narrow")) :name "buffer"))

(defconst C4/config (expand-file-name "C4.org" user-emacs-directory)
  "The central C4 config file.")

(defun C4/open-config ()
  "Open C4 configuration Org file."
  (interactive)
  (find-file C4/config))

(defun C4/reload-config ()
  "Reload C4 configuration."
  (interactive)
  (load-file user-init-file))

;;; Domain: config
(ryo-modal-keys
 ;; manage
 ("SPC c"
  (("c" C4/open-config :name "open")
   ("r" C4/reload-config :name "reload")
   ;; eval
   ("e"
    (("e" eval-last-sexp :name "expression")
     ("d" eval-defun :name "defun")
     ("r" eval-region :name "region")
     ("b" eval-buffer :name "buffer")) :name "eval")) :name "C4 config"))

;;; Domain: file
(ryo-modal-keys
 ("SPC f"
  (("f" find-file :name "find")
   ("F" find-file-other-window :name "other window")) :name "file"))

;;; Domain: help
(ryo-modal-keys
 ("SPC h"
  (("F" describe-face :name "face")
   ("m" info-emacs-manual :name "Emacs manual")) :name "help"))

;;; Domain: session
(ryo-modal-keys
 ("SPC q"
  (("q" save-buffers-kill-emacs :name "quit")
   ("Q" kill-emacs :name "really quit")) :name "session"))

(defhydra C4/text-scale (:timeout 15)
  "Interactively scale text"
  ("+" text-scale-increase "inc")
  ("-" text-scale-decrease "dec")
  ("RET" nil "exit" :exit t))

;;; Domain: toggle
(ryo-modal-keys
 ("SPC t"
  (("s" C4/text-scale/body :name "text scaling")) :name "toggle"))

(defhydra C4/window-commander (:timeout 45)
  "Interactive window navigation"
  ("SPC" other-window "cycle")
  ("c" delete-window "close")
  ("C" delete-other-windows "fill frame")
  ("i" windmove-up "jump up")
  ("I" windmove-swap-states-up "swap up")
  ("M-i" windmove-delete-up "close above")
  ("k" windmove-down "jump down")
  ("K" windmove-swap-states-down "swap down")
  ("C-k" windmove-delete-down "close below")
  ("j" windmove-left "jump left")
  ("J" windmove-swap-states-left "swap left")
  ("C-j" windmove-delete-left "close left")
  ("l" windmove-right "jump right")
  ("L" windmove-swap-states-right "swap right")
  ("C-l" windmove-delete-right "close right")
  ("RET" nil "exit" :exit t))

;;; Domain: window
(ryo-modal-keys
 ("SPC w"
  (("w" other-window :name "switch")
   ("c" delete-window :name "close")
   ("C" delete-other-windows :name "close other")
   ("n"
    (("n" C4/window-commander/body :name "state: window commander")
     ("i" windmove-up :name "jump up")
     ("I" windmove-swap-states-up :name "swap up")
     ("M-i" windmove-delete-up :name "close above")
     ("k" windmove-down :name "jump down")
     ("K" windmove-swap-states-down :name "swap down")
     ("C-k" windmove-delete-down :name "close below")
     ("j" windmove-left :name "jump left")
     ("J" windmove-swap-states-left :name "swap left")
     ("C-j" windmove-delete-left :name "close left")
     ("l" windmove-right :name "jump right")
     ("L" windmove-swap-states-right :name "swap right")
     ("C-l" windmove-delete-right :name "close fright")) :name "navigator")
   ("s"
    (("s" split-window-below :name "horizontal")
     ("S" split-window-right :name "vertical")) :name "split"))
  :name "window"))

;;; Benchmark Emacs startup to debug performance
(use-package esup
  :ryo
  ("SPC c d"
   (("d" esup :name "startup")) :name "debug"))

;;; Debug init file errors
(use-package bug-hunter
  :ryo
  ("SPC c d"
   (("e" bug-hunter-init-file :name "errors"))))

;;; Check running processes in Emacs for slowdowns
(use-package explain-pause-mode
  :ryo
  ("SPC c d"
   (("p" explain-pause-top :name "processes")))
  :config
  (explain-pause-mode))

;;; Utilities for useful Emacs functions
(use-package crux
  :ryo
  ("<return>" crux-smart-open-line :name "insert new line" :exit t)
  ("<C-return>" crux-smart-open-line-above :name "insert new line above" :exit t)
  ("SPC f"
   (("x" crux-create-scratch-buffer :name "scratch")
    ("r" crux-rename-file-and-buffer :name "rename")))
  :hook
  (find-file . crux-reopen-as-root-mode))

(setq-default cursor-type 'bar) ; default cursor as bar
(setq-default frame-title-format '("%b")) ; window title is the buffer name

(setq linum-format "%4d ") ; line number format
(column-number-mode 1) ; set column number display
(show-paren-mode 1) ; show closing parens by default

(menu-bar-mode -1) ; disable the menubar
(scroll-bar-mode -1) ; disable visible scroll bar
(tool-bar-mode -1) ; disable toolbar
(tooltip-mode -1) ; disable tooltips
(set-fringe-mode 8) ; allow some space

(setq inhibit-startup-message t) ; inhibit startup message
(setq initial-scratch-message "") ; no scratch message
(setq initial-major-mode 'text-mode)
(setq visible-bell t)             ; enable visual bell
(global-auto-revert-mode t) ; autosave buffer on file change
(delete-selection-mode 1) ; Selected text will be overwritten on typing
(fset 'yes-or-no-p 'y-or-n-p) ; convert "yes" or "no" confirms to "y" and "n"

;; Show line numbers in programming modes
(add-hook 'prog-mode-hook
          (if (and (fboundp 'display-line-numbers-mode) (display-graphic-p))
              #'display-line-numbers-mode
            #'linum-mode))

;; Disable for document and terminal modes
(dolist (mode '(
                org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                vterm-mode
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Give buffers unique names
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Make some icons available
(use-package all-the-icons)

;;; Set full name and email address
(setq user-full-name "Chatman R. Jr")
(setq user-mail-address "crjr.code@protonmail.com")

;;; Better undo/redo
(use-package undo-fu
  :ryo
  ("z" undo-fu-only-undo :name "undo last edit")
  ("Z" undo-fu-only-redo :name "redo last edit")
  ("C-z" undo-fu-only-redo-all :name "restore edits to most recent state"))

;; Undo persistence
(use-package undo-fu-session
  :hook
  (prog-mode . undo-fu-session-mode)
  (text-mode . undo-fu-session-mode)
  (org-mode . undo-fu-session-mode))

;;; Expand region selections by semantic units
(use-package expand-region
  :ryo
  ("M"
   (("m" er/expand-region :name "cycle targets")
    ("s" er/mark-sentence :name "mark sentence")
    ("[" er/mark-inside-pairs :name "mark between delimiters")
    ("{" er/mark-outside-pairs :name "mark around delimiters")
    ("'" er/mark-inside-quotes :name "mark inside quotes")
    ("\"" er/mark-outside-quotes :name "mark around quotes"))))

;;; Better minibuffer completion
(use-package selectrum
  :config
  (selectrum-mode 1))

;;; Remember frequently used commands and queries
(use-package selectrum-prescient
  :after selectrum
  :config
  (selectrum-prescient-mode 1)
  (prescient-persist-mode 1))

;;; Partial completion queries support
(use-package orderless
  :init
  (icomplete-mode)
  :custom
  (completion-styles '(orderless)))

;;; Better search utilities
(use-package consult
  :ryo
  ("SPC ." consult-complex-command :name "query command history")
  ("C-v" consult-yank :name "paste from registry")
  ("SPC b"
   (("b" consult-buffer :name "switch")
    ("B" consult-buffer-other-window :name "other window")))
  ("SPC h" (("a" consult-apropos :name "apropos")))
  ("SPC p" (("s" consult-ripgrep :name "search")) :name "project")
  :init
  (defun find-fd (&optional dir initial)
    (interactive "P")
    (let ((consult-find-command "fd --color=never --full-path ARG OPTS"))
      (consult-find dir initial)))
  (advice-add #'register-preview :override #'consult-register-window)
  :custom
  (register-preview-delay 0)
  (register-preview-function #'consult-register-window)
  (consult-narrow-key "<"))

;;; An interface for minibuffer actions
(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

;;; Adds annotations to minibuffer interfaces
(use-package marginalia
  :after selectrum
  :init
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode)
                           (selectrum-exhibit))))
  (setq marginalia-annotators
        '(marginalia-annotators-heavy marginalia-annotators-light))
  :config
  (marginalia-mode 1))

;;; Incremental search interface similar to web browsers
(use-package ctrlf
  :ryo
  ("SPC b s"
   (("s" ctrlf-forward-literal :name "forward literal")
    ("S" ctrlf-backward-literal :name "backward literal")
    ("f" ctrlf-forward-fuzzy :name "forward fuzzy")
    ("F" ctrlf-backward-fuzzy :name "backward fuzzy")
    ("r" ctrlf-forward-regexp :name "forward regexp")
    ("R" ctrlf-backward-regexp :name "backward regexp")) :name "isearch")
  :hook
  (text-mode . ctrlf-mode)
  (prog-mode . ctrlf-mode)
  (org-mode . ctrlf-mode))

;;; Lightweight mode line goodness
(use-package smart-mode-line
  :init
  (setq sml/theme 'respectful)
  (setq sml/no-confirm-load-theme t)
  (setq sml/name-width '(16 . 32))
  (setq sml/mode-width 'full)
  (setq rm-blacklist nil)
  (setq rm-whitelist '(" ryo"))
  :config
  (sml/setup)
  (add-to-list 'sml/replacer-regexp-list '("^~/.config/emacs/" ":Emacs:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/Workbench/" ":Code:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/Org/" ":Org:") t))

;;; Help documentation enhancements
(use-package helpful
  :ryo
  ("SPC h"
   (("h" helpful-at-point :name "symbol at point")
    ("f" helpful-function :name "function")
    ("c" helpful-command :name "command")
    ("C" helpful-callable :name "callable")
    ("v" helpful-variable :name "variable")
    ("k" helpful-key :name "keybinding"))))

;;; Universal editor settings
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;;; Rich terminal experience
(use-package vterm
  :ryo
  ("SPC '" vterm :name "vterm: open terminal from current dir"))

;;; Set some variables for my settings and styles
(defconst C4/font "Input Sans-13"
  "The default UI and fixed-width font for my config.")

(defconst C4/document-font "Merriweather-16"
  "The default variable pitch font for my config.")

;; By default, use Input Sans family at 13px
(set-face-attribute 'default nil :font C4/font)

;; Code font is the same as UI font
(set-face-attribute 'fixed-pitch nil :font C4/font)

;; Set default document font as Merriweather family at 16px
(set-face-attribute 'variable-pitch nil :font C4/document-font)

;;; Disable the fringe background
(set-face-attribute 'fringe nil
                    :background nil)

;;; Setup poet-themes
(use-package poet-theme)

;; Light monochrome by default
(load-theme 'poet-monochrome t)

;; Load in dark monochrome for quick toggling
(load-theme 'poet-dark-monochrome t t)

;;; Set variables for my root project directory and GitHub username
(setq C4/project-root '("~/Code"))
(setq C4/gh-user "cr-jr")

;;; Project management
(use-package projectile
  :ryo
  ("SPC p"
   (("p" projectile-switch-project :name "switch")
    ("'" projectile-run-vterm :name "open terminal")
    ("f" projectile-find-file :name "find file")))
  :hook
  (ryo-modal-mode . projectile-mode)
  :custom
  (projectile-project-search-path C4/project-root)
  (projectile-sort-order 'recently-active)
  (projectile-switch-project-action #'projectile-dired)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;;; Magical Git management
(use-package magit
  :ryo
  ("SPC g"
   (("g" magit :name "status")
    ("c" magit-commit :name "commit")
    ("d" magit-diff :name "diff")
    ("i" magit-init :name "init")
    ("p" magit-push :name "push")
    ("P" magit-pull :name "pull")
    ("r" magit-remote :name "remote")
    ("s" magit-stage :name "stage")
    ("S" magit-stage-file :name "stage current file")) :name "git")
  :commands (magit magit-status)
  :custom
  (magit-completing-read-function #'selectrum-completing-read)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;;; A Magit extension to manage Git forges (GitHub, GitLab) from Magit
(use-package forge
  :after magit
  :ryo
  ("SPC g f"
   (("f" forge-pull :name "pull")
    ("F" forge-fork :name "fork repo")
    ("i" forge-list-issues :name "issues")
    ("I" forge-create-issue :name "create issue")) :name "forge")
  :custom
  (auth-sources '("~/.authinfo"))
  :config
  (ghub-request "GET" "/user" nil
                :forge 'github
                :host "api.github.com"
                :username C4/gh-user
                :auth 'forge))

  ;;; Show how files have changed between commits
  (use-package diff-hl
    :after magit
    :hook
    (magit-pre-refresh . diff-hl-magit-pre-refresh)
    (magit-post-refresh . diff-hl-magit-post-refresh)
    :config
    (global-diff-hl-mode 1))

;;; Variables for Org Mode configuration
(setq C4/org-root-path "~/Documents/Org")
(setq C4/org-agenda-files '("Tasks.org" "Projects.org"))

(defhydra org-trek (:timeout 30)
  "A transient mode to logically browse an Org file"
  ("h" org-forward-heading-same-level "jump to next heading (same level)")
  ("H" org-backward-heading-same-level "jump to prev heading (same level)")
  ("s" org-babel-next-src-block "jump to next src block")
  ("S" org-babel-previous-src-block "jump to prev src block")
  ("v" org-next-visible-heading "jump to next heading")
  ("V" org-previous-visible-heading "jump to prev heading")
  ("RET" nil "exit state: org-trek" :exit t))

;;; Org setup
(use-package org
  :straight org-plus-contrib
  :ryo
  ("SPC o" nil :name "org")
  (:mode 'org-mode)
  ("SPC o a"
   (("a" org-agenda-list :name "weekly")
    ("f" org-agenda :name "full")
    ("t" org-set-tags-command :name "tags")) :name "agenda")
  ("SPC o b"
   (("b" org-insert-link :name "link")
    ("c" org-capture :name "capture")
    ("r" org-refile :name "refile")
    ("n"
     (("n" org-toggle-narrow-to-subtree :name "subtree")
      ("b" org-narrow-to-block :name "block")
      ("e" org-narrow-to-element :name "element")) :name "narrow")
    ("s" org-trek/body :name "state: org-trek")) :name "buffer")
  ("SPC o d"
   (("d" org-deadline :name "deadline")
    ("s" org-schedule :name "schedule")) :name "date")
  ("SPC o s"
   (("s" org-edit-special :name "edit")
    ("e" org-babel-execute-src-block :name "execute")
    ("t" org-babel-tangle :name "tangle")) :name "special")
  (:mode 'org-src-mode)
  ("SPC o o" org-edit-src-exit :name "exit")
  ("SPC o O" org-edit-src-abort :name "without saving")
  :hook
  (org-mode . variable-pitch-mode)
  (org-mode . org-indent-mode)
  (org-mode . visual-line-mode)
  (org-mode . auto-fill-mode)
  :custom-face
  (org-meta-line ((t (:extend t))))
  (org-block-begin-line ((t (:extend t))))
  (org-block ((t (:extend t))))
  (org-block-end-line ((t (:extend t))))
  :config
  (setq org-ellipsis " ➕")
  (setq org-directory C4/org-root-path)
  (setq line-spacing 0.25)
  (setq header-line-format " ")
  
  ;;; Org agenda flow
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  
  (setq org-agenda-files C4/org-agenda-files)
  
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence
           "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)"
           "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
  
  (setq org-refile-targets
        '(("Archive.org" :maxlevel . 1)
          ("Projects.org" :maxlevel . 1)
          ("Tasks.org" :maxlevel . 1)))
  
  (setq org-tag-alist
        '((:startgroup)
          ("@product" . ?P)
          ("@experiment" . ?E)
          ("@resource" . ?R)
          ("@learning" . ?L)
          ("@teaching" . ?T)
          (:endgroup)
          ("prototyping" . ?p)
          ("developing" . ?d)
          ("documenting" . ?D)
          ("testing" . ?t)
          ("refactoring" . ?r)))
  
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))
  
    ("P" "Products" tags-todo "@product")
          ("E" "Experiments" tags-todo "@experiment")
          ("R" "Resources" tags-todo "@resource")
          ("L" "Learning" tags-todo "@learning")
          ("T" "Teaching" tags-todo "@teaching")
  
          ("s" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on External")
       (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "Under Review")
       (org-agenda-files org-agenda-files)))
            (todo "PLAN"
                  ((org-agenda-overriding-header "Planning")
       (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
                  ((org-agenda-overriding-header "Project Backlog")
       (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Ready for Work")
       (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
       (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Completed Projects")
       (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Cancelled Projects")
       (org-agenda-files org-agenda-files)))))))
  
  
  ;;; Org template definitions
  (setq org-capture-templates
        `(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "Tasks.org" "Inbox")
           "* TODO %?\n %U\n %a\n %i" :empty-lines 1)))
  
  
  ;;; Org-babel setup
  (setq org-src-fontify-natively t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-babel-lisp-eval-fn "sly-eval")
  
  ;;; Supported languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (lisp . t)
     (C . t)
     (js . t)))
  
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (add-to-list 'org-refile-targets '("C4.org" :maxlevel . 3)))

;;; Org Superstar makes your bullets bang louder
(use-package org-superstar
  :after org
  :hook
  (org-mode . org-superstar-mode)
  :custom-face
  (org-superstar-leading ((t (:inherit 'org-hide))))
  :init
  (setq org-superstar-headline-bullets-list
        '(" " " " " " " " " " " " " " " ")))

;;; visual-fill-column does just enough UI adjustment for Org Mode
(use-package visual-fill-column
  :custom
  (visual-fill-column-width 120)
  (visual-fill-column-center-text t)
  :hook
  (visual-line-mode . visual-fill-column-mode)
  :config
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

;;; Add support for a table of contents
(use-package toc-org
  :after org
  :hook
  (org-mode . toc-org-mode))

;;; Journal file header
(defun C4/org-journal-file-header (time)
  "Custom function to create a journal header."
  (concat
   (pcase org-journal-file-type
     (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
     (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
     (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
     (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))

;;; Add journaling support to Org Mode
(use-package org-journal
  :ryo
  ("SPC o j"
   (("j" org-journal-new-entry :name "new")
    ("J" org-journal-read-entry :name "read")
    ("n" org-journal-next-entry :name "next")
    ("p" org-journal-previous-entry :name "prev")
    ("s" org-journal-search :name "search")
    ("c" calendar :name "calendar")) :name "journal")
  :custom
  ;; Files
  (org-journal-dir "~/Documents/Org/Notes/Fleeting/")
  (org-journal-file-format "%V|%F")

  ;; Entries
  (org-journal-file-header 'C4/org-journal-file-header)

  ;; Org agenda integration
  (org-journal-enable-agenda-integration t))



;;; A full on parser in Emacs with highlighting definitions
(use-package tree-sitter
  :config
  (global-tree-sitter-mode 1))

;; A collection of supported tree-sitter languages
(use-package tree-sitter-langs
  :after tree-sitter)

;;; Set syntax highlighting faces

;; set comment face
(set-face-attribute 'font-lock-comment-face nil :font "Input Serif Narrow-13:italic" :weight 'extra-light)

;; set keyword face
(set-face-attribute 'font-lock-keyword-face nil :font "Input Sans Compressed-13" :weight 'bold)

;; set constants face
(set-face-attribute 'font-lock-constant-face nil :inherit 'font-lock-keyword-face)

;; set built-in face
(set-face-attribute 'font-lock-builtin-face nil :inherit 'font-lock-keyword-face)

;; set function name face
(set-face-attribute 'font-lock-function-name-face nil :font "Input Sans" :weight 'black)

;; set string face
(set-face-attribute 'font-lock-string-face nil :font "Input Serif Compressed" :weight 'normal)

;; set variable name face
(set-face-attribute 'font-lock-variable-name-face nil :inherit 'font-lock-function-name-face)

  ;;; When I'm knee deep in parens
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (prog-mode . prettify-symbols-mode))

;;; Code linting package that flies
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;;; Universal code formatting package
(use-package apheleia
  :straight
  '(apheleia
    :host github
    :repo "raxod502/apheleia")
  :hook (prog-mode . apheleia-mode))

;;; Autopair delimiters
(use-package smartparens
  :hook
  (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

;;; Automatic indentation for my sanity
(use-package aggressive-indent
  :hook
  (prog-mode . aggressive-indent-mode))

;;; Code autocomplete with Company
(use-package company
  :config
  (setq
   company-idle-delay 0.25
   company-minimum-prefix-length 1
   company-selection-wrap-around t
   company-show-numbers t
   company-dabbrev-downcase nil
   company-echo-delay 0
   company-tooltip-limit 14
   company-transformers '(company-sort-by-occurrence)
   company-begin-commands '(self-insert-command))
  (global-company-mode 1))

;;; A nice Company interface
(use-package company-box
  :hook (company-mode . company-box-mode))

;;; Language Server Protocol package for rich IDE features
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred))

;; UI enhancements for lsp-mode
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode)

(defun C4/create-one-liner ()
  "Create a one line snippet to expand immediately."
  (interactive)
  (aya-create-one-line))

(defun C4/expand-snippet ()
  "Expand the last created snippet and fill it in."
  (interactive)
  (aya-expand))

(defun C4/save-snippet ()
  "Save the created snippet to database."
  (interactive)
  (aya-persist-snippet)
  (yas/reload-all))

;;; Snippet support

;; Setup YASnippet
(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode))

;; Setup Auto-YASnippet
(use-package auto-yasnippet
  :ryo
  (:mode 'prog-mode)
  ("SPC s"
   (("s" aya-create :name "create")
    ("e" C4/expand-snippet :name "expand" :exit t)
    ("w" C4/save-snippet :name "save")) :name "snippet"))

;;; Lang: Emacs Lisp

;; Inline Emacs Lisp evaluation results
(use-package eros
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :ryo
  (:mode 'emacs-lisp-mode)
  ("SPC l"
   (("e"
     (("e" eros-eval-last-sexp :name "expression")
      ("d" eros-eval-defun :name "defun")) :name "eval")) :name "emacs-lisp")
  :hook
  (emacs-lisp-mode . eros-mode)
  (lisp-interaction-mode . eros-mode))

;;; Lang: Common Lisp

;; Setup SLY
(use-package sly
  :mode ("\\.lisp\\'" . lisp-mode)
  :interpreter ("sbcl" . lisp-mode)
  :ryo
  (:mode 'sly-mode)
  ("SPC l"
   ;; Connections
   (("C"
     (("c" sly :name "invoke")
      ("l" sly-list-connections :name "list active")
      (">" sly-next-connection :name "next")
      ("<" sly-prev-connection :name "prev"))
     :name "connections")

    ;; Annotations
    ("a"
     (("a" sly-next-note :name "next")
      ("A" sly-previous-note :name "prev")
      ("C-a" sly-remove-notes :name "remove all")) :name "annotations")

    ;; Docs
    ("d"
     (("d" sly-autodoc-mode :name "autodoc toggle")
      ("m" sly-autodoc-manually :name "autodoc manually")
      ("a" sly-arglist :name "arglist")
      ("s" sly-info :name "SLY manual")) :name "docs")

    ;; Compiling
    ("c"
     (("c" sly-compile-defun :name "defun")
      ("r" sly-compile-region :name "region")
      ("f" sly-compile-file :name "file")
      ("F" sly-compile-and-load-file :name "and load")) :name "compile")
    ("E" next-error :name "show errors")

    ;; Evaluation
    ("e"
     (("e" sly-eval-last-expression :name "expression")
      ("E" sly-pprint-eval-last-expression :name "to buffer")
      ("i" sly-interactive-eval :name "interactive")
      ("d" sly-eval-defun :name "defun")
      ("r" sly-eval-region :name "region")
      ("R" sly-pprint-eval-region :name "to buffer")
      ("b" sly-eval-buffer :name "buffer")) :name "eval")

    ;; Files
    ("f" sly-load-file :name "load file")

    ;; Macros
    ("m"
     (("m" sly-expand-1 :name "expand")
      ("M" sly-macroexpand-all :name "all")
      ("c" sly-compiler-macroexpand-1 :name "compiler expand")
      ("C" sly-compiler-macroexpand :name "repeatedly")
      ("f" sly-format-string-expand :name "format string")
      ("r" sly-macroexpand-1-inplace :name "recursive expand")
      ("R" sly-macroexpand-again :name "repeat last")
      ("u" sly-macro-expand-undo :name "undo last")) :name "macro")

    ;; Definitions
    ("d"
     (("d" sly-describe-symbol :name "symbol")
      ("f" sly-describ-function :name "function")
      ("a" sly-apropos :name "apropos")
      ("A" sly-apropos-all :name "with globals")
      ("C-a" sly-apropos-package :name "package")
      ("h" sly-hyperspec-lookup :name "hyperspec lookup")
      ("H" sly-hyperspec-lookup-format :name "format")
      ("C-h" sly-hyperspec-lookup-reader-macro :name "reader macro"))
     :name "definitions")

    ;; Cross-reference
    ("x"
     (("x" sly-edit-uses :name "symbol")
      ("c" sly-who-calls :name "callers")
      ("C" sly-calls-who :name "callees")
      ("g" sly-who-references :name "global")
      ("G" sly-who-binds :name "global bindings")
      ("C-g" sly-who-sets :name "global assignments")
      ("m" sly-who-macroexpands :name "macroexpansions")
      ("M" sly-who-specializes :name "methods")) :name "x-ref"))
   :name "common-lisp")
  :hook
  (lisp-mode . sly-mode)
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl")

  (sly))

;;; Lang: Racket

;; Initialize racket-mode
(use-package racket-mode
  :mode ("\\.rkt\\'" . racket-mode)
  :interpreter ("racket" . racket-mode)
  :ryo
  (:mode 'racket-mode)
  ("SPC l"
   ;; Run
   (("r"
     (("r" racket-run :name "run")
      ("R" racket-run-and-switch-repl :name "and switch to REPL")
      ("m" racket-run-module-at-point :name "module")) :name "program")

    ;; Eval
    ("e"
     (("e" racket-send-last-sexp :name "exprssion")
      ("d" racket-send-definition :name "definition")
      ("r" racket-send-region :name "region")) :name "eval")

    ;; Testing
    ("t"
     (("t" racket-test :name "run")
      ("z" racket-fold-all-tests :name "fold")
      ("Z" racket-unfold-all-tests :name "unfold")) :name "tests")) :name "racket")
  :custom
  (racket-program "~/.asdf/shims/racket")
  :init
  (setq tab-always-indent 'complete)
  :hook
  (racket-mode . racket-xp-mode)
  (racket-mode . racket-smart-open-bracket-mode)
  (racket-mode . racket-unicode-input-method-enable)
  (racket-repl-mode . racket-unicode-input-method-enable))

;; Racket Org mode support
(use-package ob-racket
  :straight (ob-racket :host github :repo "DEADB17/ob-racket")
  :after org
  :config
  (append '((racket . t) (scribble . t)) org-babel-load-languages))

;;; Lang: JavaScript

;; Setup js2-mode
(use-package js2-mode
  :mode ("\\.js\\'" . js-mode)
  :interpreter ("node" . js-mode)
  :hook
  (js-mode . js2-minor-mode)
  (js-mode . lsp-mode))

;; Setup js-comint.el
(use-package js-comint
  :ryo
  (:mode 'js-mode)
  ("SPC l"
   (("e"
     (("e" js-send-last-sexp :name "expression")
      ("E" js-send-last-sexp-and-go :name "and switch to REPL")
      ("r" js-send-region :name "region")
      ("R" js-send-region-and-go :name "and switch to REPL")
      ("b" js-send-buffer :name "buffer")
      ("B" js-send-buffer-and-go :name "and switch to REPL")) :name "eval")
    ("r"
     (("r" js-comint-start-or-switch-to-repl :name "run")
      ("R" js-reset-repl :name "reset")) :name "program"))
   :name "javascript")
  :init
  (setq inferior-js-program-command "~/.asdf/shims/node"))

;; Setup json-mode
(use-package json-mode
  :mode
  ("\\.json\\'" . json-mode)
  ("\\.jsonp\\'" . json-mode))

;;; Lang: TypeScript

;; Setup Tide
(use-package tide
  :after
  (typescript-mode company flycheck)
  :hook
  (typescript-mode . lsp-mode)
  (typescript-mode . tide-setup)
  (typescript-mode . tide-hl-identifier-mode)
  (before-save . tide-format-before-save))

;; Org babel support
(use-package ob-typescript
  :after org
  :config
  (append '((typescript . t)) org-babel-load-languages))

;;; Lang: HTML/CSS/Web

;; Setup skewer-mode
(use-package skewer-mode
  :ryo
  (:mode 'skewer-mode)
  ("SPC l s"
   (("s" skewer-load-buffer :name "load")
    ("c" run-skewer :name "connect")
    ("C" skewer-run-phantomjs :name "headless")
    ("e" skewer-eval-last-expression :name "evaluate expression")
    ("E" skewer-eval-defun :name "evaluate function")
    ("r" skewer-repl :name "run")) :name "skewer")
  (:mode 'skewer-html-mode)
  ("SPC l"
   (("l" skewer-html-eval-tag :name "eval")
    ("e" skewer-html-fetch-selector-into-buffer :name "expand innerHTML"))
   :name "HTML")
  (:mode 'skewer-css-mode)
  ("SPC l"
   (("l" skewer-css-eval-current-declaration :name "declaration")
    ("L" skewer-css-eval-current-rule :name "rule")
    ("C-l" skewer-css-eval-buffer :name "buffer")
    ("M-l" skewer-css-clear-all :name "clear all"))
   :name "CSS")
  :hook
  (js-mode . skewer-mode)
  (html-mode . skewer-html-mode)
  (css-mode . skewer-css-mode))

;; Setup emmet-mode
(use-package emmet-mode
  :bind
  ("TAB" . emmet-expand-line)
  :hook
  (html-mode . emmet-mode)
  (css-mode . emmet-mode))

;; Setup impatient-mode
(use-package impatient-mode
  :ryo
  (:mode 'impatient-mode)
  ("SPC l"
   (("c" httpd-start :name "connect")
    ("C" httpd-stop :name "disconnect")
    ("C-c" httpd-serve-directory :name "serve from dir")))
  :hook
  (html-mode . impatient-mode))

;; Add support for Org babel
(use-package ob-browser
  :after org
  :config
  (append '((browser . t)) org-babel-load-languages))

;;; Initialize EXWM if GUI Emacs
(use-package exwm
  :if window-system
  :ryo
  (:mode 'exwm-mode)
  ("s-SPC"
   (("SPC" exwm-workspace-switch-to-buffer :name "switch buffer")
    ("r" exwm-reset :name "reset")
    ("q" exwm-restart :name "restart")
    ("Q" kill-emacs :name "quit session")
    ("w" exwm-workspace-switch :name "switch workspace")
    ("W" exwm-workspace-swap :name "swap workspace")
    ("o" exwm-workspace-move-window :name "move window to workspace")
    ("f" exwm-floating-toggle-floating :name "toggle floating window")
    ("F" exwm-layout-toggle-fullscreen :name "toggle fullscreen")
    ("k" exwm-layout-toggle-keyboard :name "toggle keyboard state")
    ("m" exwm-layout-toggle-mode-line :name "toggle mode line")
    ("M" exwm-layout-toggle-minibuffer :name "toggle minibuffer")
    ("c" kill-this-buffer :name "kill application")
    ("C" kill-some-buffers :name "kill multiple")
    ("s" split-window-below :name "split window horizontal")
    ("S" split-window-right :name "split window vertical"))
   :name "EXWM")
  :config
  
  ;; Wallpaper setup
  (start-process-shell-command
   "nitrogen" nil "nitrogen --restore")
  

  (display-time-mode t)

  (setq exwm-workspace-number 4)
  (setq display-time-default-load-average nil)
  (setq exwm-workspace-warp-cursor t)
  (setq focus-follows-mouse t)

  
  ;;; Ensure these keys work everywhere
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\C-\
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\s-\ ))
  
    ;;; Global keys for getting around in EXWM
  (setq exwm-input-global-keys
        `(([?\s-I] . windmove-swap-states-up)
          ([?\s-i] . windmove-up)
          ([?\s-L] . windmove-swap-states-right)
          ([?\s-l] . windmove-right)
          ([?\s-K] . windmove-swap-states-down)
          ([?\s-k] . windmove-down)
          ([?\s-J] . windmove-swap-states-left)
          ([?\s-j] . windmove-left)
          ([?\s-s] . split-window-below)
          ([?\s-S] . split-window-right)
          ([?\s-r] . exwm-reset)
          ([?\s-Q] . kill-emacs)
          ([?\s-q] . exwm-restart)
          ([?\s-W] . exwm-workspace-swap)
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-D] . app-launcher-run-app)
          ([?\s-d] . (lambda (cmd)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command cmd nil cmd)))
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))
  
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  

  ;; Update window class with the buffer name
  (add-hook 'exwm-update-class-hook #'C4/exwm-update-class)

  
  ;;; Multi monitor workspaces
  (require 'exwm-randr)
  (setq exwm-randr-workspace-monitor-plist
        '(0 "LVDS" 1 "LVDS" 2 "HDMI-0" 3 "HDMI-0"))
  (start-process-shell-command "xrandr" nil
                               (concat user-emacs-directory "desktop/multihead.sh"))
  (exwm-randr-enable)
  
  
  ;;; Enable a system tray in EXWM
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 16)
  (exwm-systemtray-enable)
  )

(defun C4/exwm-update-class ()
  (exwm-workspace-rename-buffer (concat "X Window: " exwm-class-name)))

;; Application launcher
(use-package app-launcher
  :straight '(app-launcher :host github :repo "SebastienWae/app-launcher"))

;; EXWM: Desktop Environment
(use-package desktop-environment
  :after exwm
  :diminish
  :bind
  ("s-l" . windmove-right)
  :config
  (desktop-environment-mode))
