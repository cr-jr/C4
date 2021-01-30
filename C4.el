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

;;; Setup which-key for keybinding discoverability
(use-package which-key
  :custom
  (which-key-idle-delay 1.5)
  :config
  (which-key-mode))

;;; Command mode initialization
(use-package modalka
  :commands modalka-mode
  :hook
  (text-mode . modalka-mode)
  (prog-mode . modalka-mode)
  (exwm-mode . modalka-mode))

;;; Command mode setup
(use-package general
  :config
  ;; Unbind C-SPC and rebind it to toggle Command Mode
  (global-unset-key (kbd "C-SPC"))
  (general-def "C-SPC" 'modalka-mode)

  ;; Create a global definitiion key for non-prefixed Command Mode actions
  (general-create-definer C4/global-key-def
    :keymaps 'modalka-mode-map)

  ;; Create a mnemonic leader key under Command Mode
  (general-create-definer C4/leader-key-def
    :keymaps 'modalka-mode-map
    :prefix "SPC"
    :global-prefix [\s-SPC])

  ;; Command mode universals
  (C4/global-key-def
    "." '(repeat :wk "repeat last command")
    ">" '(consult-complex-command :wk "repeat command history")
    "RET" '(modalka-mode :wk "insert text"))

  ;; Command mode navigation
  (C4/global-key-def
    "w" '(scroll-down-command :wk "scroll up the buffer")
    "W" '(beginning-of-buffer :wk "jump point to beginning of buffer")
    "s" '(scroll-up-command :wk "scroll down the buffer")
    "S" '(end-of-buffer :wk "jump point to end of buffer")
    "a" '(beginning-of-line-text :wk "jump point to beginning of line")
    "A" '(beginning-of-line :wk "jump point to beginning of line [absolute]")
    "d" '(end-of-line :wk "jump point to end of line")
    "i" '(previous-logical-line :wk "previous line")
    "k" '(next-logical-line :wk "next line")
    "j" '(backward-char :wk "previous char")
    "l" '(forward-char :wk "next char")
    "h" '(backward-word :wk "jump point to previous word")
    ";" '(forward-word :wk "jump point to next word")))

;;; Setup transient mode-ish states
(use-package hydra)

(C4/leader-key-def
  "'" '(vterm :wk "open terminal from current dir")
  "SPC" '(universal-argument :wk "command modifier"))

(C4/leader-key-def
  "b" '(:ignore t :wk "buffer")
  "bb" '(consult-buffer :wk "switch")
  "bB" '(consult-buffer-other-window :wk "switch other window")
  "bd" '(kill-current-buffer :wk "kill")
  "bD" '(kill-some-buffers :wk "kill multiple")
  "bn" '(:ignore t :wk "narrow")
  "bnn" '(widen :wk "reset")
  "bnd" '(narrow-to-defun :wk "to defun")
  "bnp" '(narrow-to-page :wk "to page")
  "bnr" '(narrow-to-region :wk "to region")
  "bk" '(kill-current-buffer :wk "kill")
  "bK" '(kill-some-buffers :wk "kill multiple")
  "bs" '(:ignore t :wk "search")
  "bss" '(ctrlf-forward-literal :wk "forward literal")
  "bsS" '(ctrlf-backward-literal :wk "backward literal")
  "bsf" '(ctrlf-forward-fuzzy :wk "forward fuzzy")
  "bsF" '(ctrlf-backward-fuzzy :wk "backward fuzzy")
  "bsr" '(ctrlf-forward-regexp :wk "forward regexp")
  "bsR" '(ctrlf-backward-regexp :wk "backward regexp")
  "bw" '(save-buffer :wk "write")
  "bW" '(save-some-buffers :wk "write modified"))

(C4/leader-key-def
 "c" '(:ignore t :wk "C4 config")
 "cc" '(C4/open-config :wk "open")
 "cd" '(:ignore t :wk "debug")
 "cdd" '(C4/esup-init :wk "startup")
 "cde" '(C4/bug-hunter-init :wk "errors")
 "cdp" '(explain-pause-top :wk "processes")
 "cr" '(C4/reload-config :wk "reload")
 "ce" '(:ignore t :wk "eval")
 "cee" '(eval-last-sexp :wk "S-exp")
 "ceb" '(eval-buffer :wk "buffer")
 "ced" '(eval-defun :wk "defun")
 "cer" '(eval-region :wk "region"))

(defun C4/generated-conf ()
  (concat user-emacs-directory "C4.el"))

(defun C4/esup-init ()
  "Profiles the correct init file"
  (interactive)
  (esup (C4/generated-conf)))

(defun C4/bug-hunter-init ()
  "Debugs the correct init file"
  (interactive)
  (bug-hunter-file (C4/generated-conf)))

(defun C4/open-config ()
  "Open files in config directory."
  (interactive)
  (find-file (concat user-emacs-directory "C4.org")))

(defun C4/reload-config ()
  "Reloads the config in place."
  (interactive)
  (load-file (C4/generated-conf)))

(C4/leader-key-def
  "f" '(:ignore t :wk "file")
  "ff" '(find-file :wk "find")
  "fx" '(crux-create-scratch-buffer :wk "scratchpad")
  "fr" '(crux-rename-file-and-buffer :wk "rename"))

(C4/leader-key-def
 "h" '(:ignore t :wk "help")
 "ha" '(consult-apropos :wk "apropos")
 "hf" '(describe-function :wk "function")
 "hF" '(describe-face :wk "face")
 "hc" '(helpful-command :wk "command")
 "hv" '(describe-variable :wk "variable")
 "hk" '(helpful-key :wk "keybinding")
 "hs" '(helpful-at-point :wk "symbol at point")
 "hm" '(info-emacs-manual :wk "Emacs"))

(C4/leader-key-def
  "o" '(:ignore t :wk "org")
  "oa" '(:ignore t :wk "agenda")
  "oaa" '(org-agenda-list :wk "weekly")
  "oaf" '(org-agenda :wk "full")
  "oat" '(org-set-tags-command :wk "tags")
  "ob" '(:ignore t :wk "buffer")
  "obb" '(org-insert-link :wk "link")
  "obc" '(org-capture :wk "capture")
  "obn" '(:ignore t :wk "narrow")
  "obnn" '(org-toggle-narrow-to-subtree :wk "subtree")
  "obnb" '(org-narrow-to-block :wk "block")
  "obne" '(org-narrow-to-element :wk "element")
  "obr" '(org-refile :wk "refile")
  "obs" '(C4/org-trek/body t :wk "search")
  "od" '(:ignore t :wk "date")
  "odd" '(org-deadline :wk "deadline")
  "ods" '(org-schedule :wk "schedule")
  "os" '(:ignore t :wk "special")
  "oss" '(org-edit-special :wk "edit")
  "osx" '(org-edit-src-exit :wk "exit with edits")
  "osX" '(org-edit-src-abort :wk "exit without edits")
  "ose" '(org-babel-execute-src-block :wk "execute")
  "ost" '(org-babel-tangle :wk "tangle"))

(defhydra C4/org-trek (:timeout 10)
  "A transient mode to logically traverse an Org file."
  ("s" org-babel-next-src-block "next source block")
  ("S" org-babel-previous-src-block "previous source block")
  ("h" org-forward-heading-same-level "next heading at current level")
  ("H" org-backward-heading-same-level "previous heading at current level")
  ("v" org-next-visible-heading "next visible heading")
  ("V" org-previous-visible-heading "previous visible heading")
  ("RET" nil "exit" :exit t))

(C4/leader-key-def
 "p" '(:ignore t :wk "project")
 "p'" '(projectile-run-vterm :wk "open terminal")
 "pp" '(projectile-switch-project :wk "switch")
 "pf" '(projectile-find-file :wk "find file")
 "pg" '(:ignore t :wk "git")
 "pgg" '(magit-status :wk "status")
 "pgc" '(magit-commit :wk "commit")
 "pgd" '(magit-diff :wk "diff")
 "pgf" '(:ignore t :wk "forge")
 "pgff" '(forge-pull :wk "pull")
 "pgfF" '(forge-fork :wk "fork repo")
 "pgfi" '(forge-list-issues :wk "issues")
 "pgfI" '(forge-create-issue :wk "create issue")
 "pgi" '(magit-init :wk "init")
 "pgp" '(magit-push :wk "push")
 "pgP" '(magit-pull :wk "pull")
 "pgr" '(magit-remote :wk "remote")
 "pgs" '(magit-stage :wk "stage")
 "pgS" '(magit-stage-file :wk "stage file")
 "ps" '(consult-ripgrep :wk "search"))

(C4/leader-key-def
  "q" '(:ignore t :wk "session")
  "qq" '(save-buffers-kill-emacs :wk "quit")
  "qQ" '(kill-emacs :wk "really quit"))

(C4/leader-key-def
  "t" '(:ignore t :wk "toggle")
  "tt" '(C4/theme-switcher/body :wk "theme")
  "ts" '(C4/text-scale/body :wk "scale text")
  "tz" '(darkroom-tentative-mode :wk "zen mode"))

(defhydra C4/theme-switcher ()
  "Select a variant from main C4 themes"
  ("l" C4/light "light variant")
  ("d" C4/dark "dark variant")
  ("b" C4/black "black variant")
  ("RET" nil "exit" :exit t))

(defun C4/light ()
  "Clap on!"
  (interactive)
  (consult-theme 'minimal-light))

(defun C4/dark ()
  "Dimmer switch!"
  (interactive)
  (consult-theme 'minimal)
  (sml/apply-theme 'dark))

(defun C4/black ()
  "Clap off!"
  (interactive)
  (consult-theme 'minimal-black)
  (sml/apply-theme 'dark))

(defhydra C4/text-scale (:timeout 4)
  "Interatively scale text"
  ("+" text-scale-increase "inc")
  ("-" text-scale-decrease "dec")
  ("RET" nil "exit" :exit t))

(C4/leader-key-def
 "w" '(:ignore t :wk "window")
 "ww" '(other-window :wk "cycle windows")
 "wc" '(delete-window :wk "close")
 "wC" '(delete-other-windows :wk "fill frame")
 "wd" '(:ignore t :wk "desktop")
 "wdf" '(exwm-floating-toggle-floating :wk "floating")
 "wdF" '(exwm-layout-toggle-fullscreen :wk "fullscreen")
 "wdk" '(exwm-layout-toggle-keyboard :wk "keyboard mode")
 "wdm" '(exwm-layout-toggle-mode-line :wk "mode line")
 "wdM" '(exwm-layout-toggle-minibuffer :wk "minibuffer")
 "wn" '(:ignore t :wk "navigator")
 "wnn" '(C4/window-commander/body :wk "interactive")
 "wni" '(windmove-up :wk "jump up")
 "wnI" '(windmove-swap-states-up "swap up")
 "wnk" '(windmove-down :wk "jump down")
 "wnK" '(windmove-swap-states-down :wk "swap down")
 "wnj" '(windmove-left :wk "jump left")
 "wnJ" '(windmove-swap-states-left :wk "swap left")
 "wnl" '(windmove-right :wk "jump right")
 "wnL" '(windmove-swap-states-right :wk "swap right")
 "wnw" '(windmove-display-up :wk "open next window above")
 "wnW" '(windmove-delete-up :wk "close window above")
 "wns" '(windmove-display-down :wk "open next window below")
 "wnS" '(windmove-delete-down :wk "close window below")
 "wna" '(windmove-display-left :wk "open next window left")
 "wnA" '(windmove-delete-left :wk "close window to left")
 "wnd" '(windmove-display-right :wk "open next window right")
 "wnD" '(windmove-delete-right :wk "close window to right")
 "wo" '(:ignore t :wk "open")
 "woo" '(consult-buffer-other-window :wk "buffer")
 "wof" '(find-file-other-window :wk "file")
 "wod" '(counsel-linux-app :wk "desktop app")
 "ws" '(:ignore t :wk "split")
 "wss" '(split-window-below :wk "horizontal")
 "wsS" '(split-window-right :wk "vertical"))

(defhydra C4/window-commander (:timeout 10)
  "Interactive window navigation"
  ("SPC" other-window "cycle")
  ("c" delete-window "close")
  ("C" delete-other-windows "fill frame")
  ("i" windmove-up "jump up")
  ("I" windmove-swap-states-up "swap up")
  ("k" windmove-down "jump down")
  ("K" windmove-swap-states-down "swap down")
  ("j" windmove-left "jump left")
  ("J" windmove-swap-states-left "swap left")
  ("l" windmove-right "jump right")
  ("L" windmove-swap-states-right "swap right")
  ("w" windmove-display-up "open next above")
  ("W" windmove-delete-up "close above")
  ("s" windmove-display-down "open next below")
  ("S" windmove-delete-down "close below")
  ("a" windmove-display-left "open next to left")
  ("A" windmove-delete-left "close left")
  ("d" windmove-display-right "open next to right")
  ("D" windmove-delete-right "close right")
  ("RET" nil "exit" :exit t))

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

;;; Include and load minimal-theme collection
(use-package minimal-theme)

;; Light theme loaded and enabled by default
(load-theme 'minimal-light t)

;; Dark variants load but not wait for toggling
(load-theme 'minimal t t)
(load-theme 'minimal-black t t)

;;; Define our font
(setq C4/font "Input 13")

;;; Set font
(set-face-attribute 'default nil :font C4/font)

;;; Org Mode adjustments
(set-face-attribute 'org-level-1 nil
      :weight 'bold)

;;; Add extended unicode support
(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

;;; Disbable the fringe background
(set-face-attribute 'fringe nil
                    :background nil)

(use-package smart-mode-line
  :init
  (setq sml/theme 'light)
  (setq sml/no-confirm-load-theme t)
  (setq sml/name-width '(16 . 32))
  (setq sml/mode-width 'full)
  (setq sml/extra-filler 12)
  (setq rm-blacklist nil)
  (setq rm-whitelist '("↑"))
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
  :hook (prog-mode . tree-sitter-mode))

;; A collection of supported tree-sitter languages
(use-package tree-sitter-langs
  :after tree-sitter)

;;; Syntax: comments
(set-face-attribute 'tree-sitter-hl-face:comment nil
                    :slant 'italic)

;;Unsupported fallback
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)

;;; When I'm knee deep in parens
(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

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

;;; Code autocomplete with Company
(use-package company
  :hook (prog-mode . company-mode))

;;; A nice Company interface
(use-package company-box
  :hook (company-mode . company-box-mode))

;;; Language Server Protocol package for rich IDE features
(use-package lsp-mode
  :hook (prog-mode . lsp-deferred)
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

;;; Variables for Org Mode configuration
(setq C4/org-root-path "~/Documents/Org")
(setq C4/org-agenda-files '("Tasks.org" "Projects.org"))

;;; Org setup
(use-package org
  :init
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
  
  :config
  (org-indent-mode 1)
  (variable-pitch-mode 1)
  (auto-fill-mode 1)
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

;;; Org Superstar makes your bullets bang louder
(use-package org-superstar
  :after org
  :hook
  (org-mode . org-superstar-mode)
  :custom-face
  (org-superstar-leading ((t (:inherit 'org-hide))))
  :init
  (setq org-superstar-headline-bullets-list
        '("❖" "◈" "◆" "◇" "⚫" "⚬" " ")))

;;; Darkroom for a better writing experience
(use-package darkroom
  :hook
  (org-mode . darkroom-tentative-mode)
  :config
  (setq darkroom-text-scale-increase 1)
  (setq darkroom-margins '(32 . 32)))

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
  

  (exwm-enable))

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
