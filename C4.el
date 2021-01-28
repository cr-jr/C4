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
(defun C4/init ()
  (org-babel-tangle-file
   (concat user-emacs-directory "C4.org")
   (concat user-emacs-directory "C4.el")))

;; Always generate on load
(C4/init)

;; Setup a hook to re-tangle the config on modification.
(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook #'C4/init)))

(require 'cl-lib)

(require 'base
         (concat user-emacs-directory "modules/base.el"))
(require 'projects
         (concat user-emacs-directory "modules/projects.el"))
(require 'code
         (concat user-emacs-directory "modules/code.el"))
(require 'documents
         (concat user-emacs-directory "modules/documents.el"))
(require 'desktop
         (concat user-emacs-directory "modules/desktop.el"))

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
  (which-key-idle-delay 0.96)
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
  "q" '(:ignore t :wk "quit")
  "qq" '(save-buffers-kill-emacs :wk "and save")
  "qQ" '(kill-emacs :wk "really quit"))

(C4/leader-key-def
  "t" '(:ignore t :wk "toggle")
  "tt" '(C4/theme-switcher/body :wk "theme"))

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
  (consult-theme 'minimal))

(defun C4/black ()
  "Clap off!"
  (interactive)
  (consult-theme 'minimal-black))

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

(c4/user
  :name "Chatman R. Jr"
  :email "crjr.code@protonmail.com")

(c4/base
  :theme 'minimal-light
  :typography
  (c4/typography
    :code '("Input" 13)
    :document '("Lora" 16)))

(c4/projects
  :path "~/Workbench"
  :username "cr-jr")

(c4/code)

(c4/org :path "~/Org")

(c4/desktop)
