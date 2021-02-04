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

;;; Need Org Mode to be first package installed because it builds the config.
(use-package org
  :straight org-plus-contrib)

;; Generate C4.el from the source blocks in C4.org
(defun C4/load ()
  (org-babel-tangle-file
   (concat user-emacs-directory "C4.org")
   (concat user-emacs-directory "C4.el")))

;; Always generate on load
(C4/load)

;; Setup a hook to re-tangle the config on modification.
(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook #'C4/load)))

;; Raise the garbage collection threshold high as emacs starts
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Drop it down once loaded
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 1000000)))

;; Lockfiles do more harm than good
(setq create-lockfiles nil)

;; Custom files just add clutter
(setq custom-file null-device)

;; Put temporary and data files in proper locations
(use-package no-littering
  :custom
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; Create parent dirs when opening new files
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

;;; Benchmark Emacs startup to debug performance
(use-package esup)

;;; Debug init file errors
(use-package bug-hunter)

;;; Check running processes in Emacs for slowdowns
(use-package explain-pause-mode
  :config
  (explain-pause-mode))

(use-package crux
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

;; Make some icons available
(use-package all-the-icons)

;;; Set full name and email address
(setq user-full-name "Chatman R. Jr")
(setq user-mail-address "crjr.code@protonmail.com")

;;; Better undo/redo
(use-package undo-fu)

;; Undo persistence
(use-package undo-fu-session
  :hook
  (prog-mode . undo-fu-session-mode)
  (text-mode . undo-fu-session-mode)
  (org-mode . undo-fu-session-mode))

;;; Expand region selections by semantic units
(use-package expand-region)

;;; Multiple cursors in Emacs
(use-package multiple-cursors)

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
  :after consult
  :init
  (marginalia-mode)
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode)
                           (selectrum-exhibit))))
  (setq marginalia-annotators
        '(marginalia-annotators-heavy marginalia-annotators-light)))

;;; Incremental search interface similar to web browsers
(use-package ctrlf
      :config
      (ctrlf-mode 1))

(use-package smart-mode-line
  :init
  (setq sml/theme 'light)
  (setq sml/no-confirm-load-theme t)
  (setq sml/name-width '(16 . 32))
  (setq sml/mode-width 'full)
  (setq rm-blacklist nil)
  (setq rm-whitelist '(" "))
  :config
  (sml/setup)
  (add-to-list 'sml/replacer-regexp-list '("^~/.config/emacs/" ":Emacs:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/Workbench/" ":Code:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/Org/" ":Org:") t))

;;; Help documentation enhancement
(use-package helpful)

;;; Universal editor settings
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;;; Rich terminal experience
(use-package vterm)

;;; Set some variables for my settings and styles
(setq C4/font "Input Sans-13")
(setq C4/font-bold "Input Sans Condensed-13:normal")
(setq C4/font-italic "Input Serif Condensed-13:light:italic")

(setq C4/document-font "Input Serif-13")
(setq C4/terminal-font "Input Mono-13")

;;; By default, use Input Sans family at 13px
(set-face-attribute 'default nil :font C4/font)
(set-face-attribute 'bold nil :font C4/font-bold)
(set-face-attribute 'italic nil :font C4/font-italic)
(set-face-attribute 'bold-italic nil :inherit 'bold)

;;; Code font is the same as UI font
(set-face-attribute 'fixed-pitch nil :font C4/font)

;;; Set default document font as Input Serif family at 13px
(set-face-attribute 'variable-pitch nil :font C4/document-font)

;;; Set default terminal font as Input Mono family at 13px
(set-face-attribute 'term nil :font C4/terminal-font)

;;; Some Org Mode adjustments
(set-face-attribute 'org-document-title nil :weight 'bold :inherit 'fixed-pitch)
(set-face-attribute 'org-document-info nil :inherit 'org-document-title)

(set-face-attribute 'org-level-1 nil :height 1.8 :weight 'bold :inherit 'fixed-pitch)
(set-face-attribute 'org-level-2 nil :height 1.6 :inherit 'fixed-pitch)
(set-face-attribute 'org-level-3 nil :height 1.4 :inherit 'fixed-pitch)
(set-face-attribute 'org-level-4 nil :height 1.2 :inherit 'fixed-pitch)
(set-face-attribute 'org-level-5 nil :height 1.0 :inherit 'fixed-pitch)
(set-face-attribute 'org-level-6 nil :height 0.8 :inherit 'fixed-pitch)

(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-block nil :inherit 'fixed-pitch)
(set-face-attribute 'org-block-begin-line nil :weight 'normal :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-block-end-line nil :weight 'normal :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-property-value nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-document-info-keyword nil :weight 'bold :inherit '(fixed-pitch font-lock-keyword-face))
(set-face-attribute 'org-drawer nil :inherit 'org-document-info-keyword)
(set-face-attribute 'org-special-keyword nil :inherit 'org-document-info-keyword)

;;; Disable the fringe background
(set-face-attribute 'fringe nil
                    :background nil)

;;; Include and load minimal-theme collection
(use-package minimal-theme)

;; Light theme loaded and enabled by default
(load-theme 'minimal-light t)

;; Dark variants load but not wait for toggling
(load-theme 'minimal t t)
(load-theme 'minimal-black t t)

;;; Setup which-key for keybinding discoverability
(use-package which-key
  :custom
  (which-key-idle-delay 1.5)
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
  (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist))

;;; Setup transient mode-ish states
(use-package hydra)

(defun C4/newline ()
  "Insert a newline after point"
  (interactive)
  (crux-smart-open-line nil))

(defun C4/newline-above ()
  "Insert a newline before point"
  (interactive)
  (crux-smart-open-line-above))

;;; Actions: insertion
(ryo-modal-keys
 ("q" ryo-modal-mode :name "insert at point")
 ("SPC" (("SPC" ryo-modal-mode :name "insert at point")))
 ("<return>" C4/newline :name "insert new line" :exit t)
 ("C-<return>" C4/newline-above :name "insert new line above" :exit t))

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
 ("C-i" beginning-of-buffer :name "jump point to beginning of buffer")
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

;;; Actions: undo/redo
(ryo-modal-keys
 ("z" undo-fu-only-undo :name "undo last edit")
 ("Z" undo-fu-only-redo :name "redo last edit")
 ("C-z" undo-fu-only-redo-all :name "restore edits to most recent state"))

(defun C4/mark-line ()
  "Mark the entire line"
  (interactive)
  (end-of-line)
  (set-mark-command nil)
  (beginning-of-line))

;;; Actions: marking/selecting text
(ryo-modal-keys
 ("m" set-mark-command :wk "set a mark at point")
 ("M"
  (("m" er/expand-region :name "cycle semantics")
   ("w" mark-word :name "mark word")
   ("s" er/mark-sentence :name "mark sentence")
   ("l" C4/mark-line :name "mark current line")
   ("p" mark-paragraph :name "mark paragraph")
   ("[" er/mark-inside-pairs :name "mark between delimiters")
   ("{" er/mark-outside-pairs :name "mark around delimiters")
   ("'" er/mark-inside-quotes :name "mark between quotes")
   ("\"" er/mark-outside-quotes :name "mark around quotes"))))

;;; Actions: killing/cutting text
(ryo-modal-keys
  ("x" kill-region :wk "cut selection")
  ("X" clipboard-kill-region :wk "cut selection (system)"))

;;; Actions: copy/paste
(ryo-modal-keys
  ("c" kill-ring-save :name "copy selection")
  ("C" clipboard-kill-ring-save :name "copy selection (system)")
  ("v" yank :name "paste")
  ("V" clipboard-yank :name "paste (system)")
  ("C-v" consult-yank :name "paste from registry"))

;;; Actions: deleting text
(ryo-modal-keys
  ("d" delete-char :wk "delete char after point")
  ("D"
   (("d" backward-delete-char :name "delete char before point")
    ("r" delete-region :name "delete-region"))))

;;; Set variables for my root project directory and GitHub username
(setq C4/project-root "~/Code")
(setq C4/gh-user "cr-jr")

(use-package projectile
    :config
    (projectile-mode)
    :custom
    (projectile-project-search-path C4/project-root)
    (projectile-sort-order 'recently-active)
    (projectile-switch-project-action #'projectile-dired)
    :bind-keymap
    ("C-c p" . projectile-command-map))

;;; Magical Git management
(use-package magit
  :commands (magit magit-status)
  :custom
  (magit-completing-read-function #'selectrum-completing-read)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;;; A Magit extension to manage Git forges (GitHub, GitLab) from Magit
(use-package forge
    :after magit
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

;;; A full on parser in Emacs with highlighting definitions
(use-package tree-sitter
  :config
  (global-tree-sitter-mode 1))

;; A collection of supported tree-sitter languages
(use-package tree-sitter-langs
  :after tree-sitter)

;;; Set syntax highlighting faces

;; set comment face
(set-face-attribute 'font-lock-comment-face nil :weight 'bold :inherit 'italic)

;; set keyword face
(set-face-attribute 'font-lock-keyword-face nil :inherit 'bold)

;; set constants face
(set-face-attribute 'font-lock-constant-face nil :font C4/font :weight 'black)

;; set built-in face
(set-face-attribute 'font-lock-builtin-face nil :inherit 'bold)

;; set function name face
(set-face-attribute 'font-lock-function-name-face nil :font C4/font :weight 'black)

;; set string face
(set-face-attribute 'font-lock-string-face nil :weight 'normal :slant 'normal :inherit 'italic)

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
  :hook (prog-mode . company-mode))

;;; A nice Company interface
(use-package company-box
  :hook (company-mode . company-box-mode))

;;; Language Server Protocol package for rich IDE features
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "")
  :hook
  (prog-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred))

;; UI enhancements for lsp-mode
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode)

;;; The debugging complement to LSP
(use-package dap-mode
  :hook
  (prog-mode . dap-mode)
  (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra))))

;;; Snippet support

;; Setup YASnippet
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; Setup snippet collection
(use-package yasnippet-snippets)

;; Setup Auto-YASnippet
(use-package auto-yasnippet)

;;; Variables for Org Mode configuration
(setq C4/org-root-path "~/Documents/Org")
(setq C4/org-agenda-files '("Tasks.org" "Projects.org"))

;;; Org setup
(use-package org
  :hook
  (org-mode . variable-pitch-mode)
  (org-mode . visual-line-mode)
  (org-mode . org-indent-mode)
  (org-mode . auto-fill-mode)
  :config
  (setq org-ellipsis " ↴")
  (setq org-directory C4/org-root-path)
  
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
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (js . t)))
  
  (setq org-src-fontify-natively t)
  (setq org-confirm-babel-evaluate nil)
  
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
        '("⚀" "⚁" "⚂" "⚃" "⚄" "⚅")))

;;; visual-fill-column does just enough UI adjustment
;;; for Org Mode
(use-package visual-fill-column
  :custom
  (visual-fill-column-width 120)
  (visual-fill-column-center-text t)
  :hook
  (visual-line-mode . visual-fill-column-mode)
  :config
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

;;; Initialize EXWM if GUI Emacs
(use-package exwm
  :if window-system
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
  (exwm-workspace-rename-buffer (concat "X: " exwm-class-name)))

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
