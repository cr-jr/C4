;; [[file:C4.org::*Initial Settings][Initial Settings:1]]
(setq-default cursor-type 'bar) ; default cursor as bar
(setq-default frame-title-format '("%b")) ; window title is the buffer name

(setq linum-format "%4d ") ; line number format
(column-number-mode 1) ; set column number display
(show-paren-mode 1) ; show closing parens by default

(menu-bar-mode -1) ; disable the menubar
(scroll-bar-mode -1) ; disable the scroll bar
(set-fringe-mode 8) ; Set fringe
(tool-bar-mode -1) ; disable toolbar
(tooltip-mode -1) ; disable tooltips

(setq inhibit-startup-message t) ; inhibit startup message
(setq initial-scratch-message "") ; no scratch message
(setq initial-major-mode 'text-mode) ; set scratch to generic text mode
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
;; Initial Settings:1 ends here

;; [[file:C4.org::*Theme][Theme:1]]
;; Setup ewal
(use-package ewal
  :init
  (setq ewal-use-built-in-always-p nil)
  (setq ewal-use-built-in-on-failure-p t)
  (setq ewal-built-in-palette "sexy-material")
  (setq ewal-json-file "~/.cache/wal/colors.json")
  :config (ewal-load-colors))

;; Load in themes
(use-package ewal-doom-themes
  :config (load-theme 'ewal-doom-one t))
;; Theme:1 ends here

;; [[file:C4.org::*Typography][Typography:1]]
;; By default, use Cousine
(set-face-attribute 'default nil :font "Cousine-12")

;; Code font is the same as default font, but slightly larger
(set-face-attribute 'fixed-pitch nil :font "Cousine-12")

;; Org mode font is a nice serif
(set-face-attribute 'variable-pitch nil :font "Noto Serif-14")

;; Set monospace font to correctly render linum and bold to track position
(set-face-attribute 'line-number nil :font "Cousine")
(set-face-attribute 'line-number-current-line nil
        :font "Cousine Bold" :foreground (ewal-load-color 'white))
;; Typography:1 ends here

;; [[file:C4.org::*UI][UI:1]]
;;; Disable the fringe background
(set-face-attribute 'fringe nil
                    :background nil)

;;; Eliminate all mode line decorations
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
;; UI:1 ends here

;; [[file:C4.org::*User Identity][User Identity:1]]
;;; Set full name and email address
(setq user-full-name "Chatman R. Jr")
(setq user-mail-address "crjr.code@protonmail.com")
;; User Identity:1 ends here

;; [[file:C4.org::*which-key][which-key:1]]
;;; Setup which-key for keybinding discoverability
(use-package which-key
  :custom
  (which-key-idle-delay 1.5)
  (which-key-enable-extended-define-key t)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  :config
  (which-key-mode))
;; which-key:1 ends here

;; [[file:C4.org::*ryo-modal][ryo-modal:1]]
;;; Command mode initialization
(use-package ryo-modal
  :commands (ryo-modal-mode)
  :bind
  ("C-SPC" . ryo-modal-mode)
  ("<menu>" . ryo-modal-mode)
  :hook
  (text-mode . ryo-modal-mode)
  (prog-mode . ryo-modal-mode)
  :config
  ;; which-key integration
  (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist)

  ;; Set activated cursor color
  (setq ryo-modal-cursor-color (ewal-load-color 'red))

  ;; C-i needs to be its own keybinding
  (keyboard-translate ?\C-i ?\M-i))
;; ryo-modal:1 ends here

;; [[file:C4.org::*hydra][hydra:1]]
;;; Setup transient mode-ish interfaces
(use-package hydra)
;; hydra:1 ends here

;; [[file:C4.org::*Exiting Command Mode][Exiting Command Mode:1]]
;;; Actions: insertion
(ryo-modal-keys
 ("q" ryo-modal-mode :name "insert at point")
 ("SPC SPC" ryo-modal-mode :name "insert at point"))
;; Exiting Command Mode:1 ends here

;; [[file:C4.org::*Modifiers][Modifiers:1]]
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
;; Modifiers:1 ends here

;; [[file:C4.org::*Movement][Movement:1]]
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
;; Movement:1 ends here

;; [[file:C4.org::*Marking/selecting][Marking/selecting:1]]
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
;; Marking/selecting:1 ends here

;; [[file:C4.org::*Killing/cutting][Killing/cutting:1]]
;;; Actions: killing/cutting text
(ryo-modal-keys
 ("x" kill-region :wk "cut selection")
 ("X" clipboard-kill-region :wk "cut selection (system)"))
;; Killing/cutting:1 ends here

;; [[file:C4.org::*Copy/paste][Copy/paste:1]]
;;; Actions: copy/paste
(ryo-modal-keys
 ("c" kill-ring-save :name "copy selection")
 ("C" clipboard-kill-ring-save :name "copy selection (system)")
 ("v" yank :name "paste")
 ("V" clipboard-yank :name "paste (system)"))
;; Copy/paste:1 ends here

;; [[file:C4.org::*Deletion][Deletion:1]]
;;; Actions: deleting text
(ryo-modal-keys
 ("d" delete-char :wk "delete char after point")
 ("D"
  (("d" backward-delete-char :name "delete char before point")
   ("r" delete-region :name "delete-region"))))
;; Deletion:1 ends here

;; [[file:C4.org::*Modifiers][Modifiers:1]]
;;; Command modifiers
(ryo-modal-keys
 ("SPC u" universal-argument :name "command modifier"))
;; Modifiers:1 ends here

;; [[file:C4.org::*Buffer (=b=)][Buffer (=b=):1]]
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
;; Buffer (=b=):1 ends here

;; [[file:C4.org::*Config (=c=)][Config (=c=):1]]
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
;; Config (=c=):1 ends here

;; [[file:C4.org::*File (=f=)][File (=f=):1]]
;;; Domain: file
(ryo-modal-keys
 ("SPC f"
  (("f" find-file :name "find")
   ("F" find-file-other-window :name "other window")
   ("d" dired :name "directory")) :name "file"))
;; File (=f=):1 ends here

;; [[file:C4.org::*Help (=h=)][Help (=h=):1]]
;;; Domain: help
(ryo-modal-keys
 ("SPC h"
  (("F" describe-face :name "face")
   ("m" info-emacs-manual :name "Emacs manual")) :name "help"))
;; Help (=h=):1 ends here

;; [[file:C4.org::*Session (=q=)][Session (=q=):1]]
;;; Domain: session
(ryo-modal-keys
 ("SPC q"
  (("q" save-buffers-kill-emacs :name "quit")
   ("Q" kill-emacs :name "really quit")) :name "session"))
;; Session (=q=):1 ends here

;; [[file:C4.org::*Toggle (=t=)][Toggle (=t=):1]]
(defhydra C4/text-scale (:timeout 15)
  "Interactively scale text"
  ("+" text-scale-increase "inc")
  ("-" text-scale-decrease "dec")
  ("RET" nil "exit" :exit t))

;;; Domain: toggle
(ryo-modal-keys
 ("SPC t"
  (("s" C4/text-scale/body :name "text scaling")) :name "toggle"))
;; Toggle (=t=):1 ends here

;; [[file:C4.org::*Window (=w=)][Window (=w=):1]]
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
;; Window (=w=):1 ends here

;; [[file:C4.org::*Garbage collection][Garbage collection:1]]
;;; Raise the garbage collection threshold high as emacs starts
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;;; Drop it down once loaded
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 1000000)))
;; Garbage collection:1 ends here

;; [[file:C4.org::*esup][esup:1]]
;;; Benchmark Emacs startup to debug performance
(use-package esup
  :ryo
  ("SPC c d"
   (("d" esup :name "startup")) :name "debug"))
;; esup:1 ends here

;; [[file:C4.org::*elisp-bug-hunter][elisp-bug-hunter:1]]
;;; Debug init file errors
(use-package bug-hunter
  :ryo
  ("SPC c d"
   (("e" bug-hunter-init-file :name "errors"))))
;; elisp-bug-hunter:1 ends here

;; [[file:C4.org::*explain-pause-mode][explain-pause-mode:1]]
;;; Check running processes in Emacs for slowdowns
(use-package explain-pause-mode
  :ryo
  ("SPC c d"
   (("p" explain-pause-top :name "processes")))
  :config
  (explain-pause-mode))
;; explain-pause-mode:1 ends here

;; [[file:C4.org::*Inhibit lockfiles and custom files][Inhibit lockfiles and custom files:1]]
;;; Lockfiles do more harm than good
(setq create-lockfiles nil)

;;; Custom files just add clutter
(setq custom-file null-device)
;; Inhibit lockfiles and custom files:1 ends here

;; [[file:C4.org::*no-littering][no-littering:1]]
;;; Put temporary and data files in proper locations
(use-package no-littering
  :custom
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))
;; no-littering:1 ends here

;; [[file:C4.org::*Create parent directories automatically][Create parent directories automatically:1]]
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
;; Create parent directories automatically:1 ends here

;; [[file:C4.org::*whitespace-cleanup-mode][whitespace-cleanup-mode:1]]
;;; Clean up whitespace in all major modes on save
(use-package whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode t))
;; whitespace-cleanup-mode:1 ends here

;; [[file:C4.org::*mood-line][mood-line:1]]
;;; Lightweight mode line goodness
(use-package mood-line :config (mood-line-mode))
;; mood-line:1 ends here

;; [[file:C4.org::*helpful][helpful:1]]
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
;; helpful:1 ends here

;; [[file:C4.org::*editorconfig][editorconfig:1]]
;;; Universal editor settings
(use-package editorconfig
  :config
  (editorconfig-mode 1))
;; editorconfig:1 ends here

;; [[file:C4.org::*eshell][eshell:1]]
;; Enhanced eshell
(use-package eshell-prompt-extras
  :custom
  (eshell-highlight-prompt nil)
  (eshell-prompt-function 'epe-theme-lambda))

;; Easy shell access
(use-package shell-pop
  :ryo
  ("SPC '" shell-pop :name "pop a terminal")
  ("SPC \"" term :name "open terminal")
  :custom
  (shell-pop-window-size 30)
  (shell-pop-shell-type (quote ("eshell" "*Eshell*" (lambda nil (eshell))))))
;; eshell:1 ends here

;; [[file:C4.org::*crux][crux:1]]
;;; Utilities for useful Emacs functions
(use-package crux
  :ryo
  ("<return>" crux-smart-open-line :name "insert new line" :exit t)
  ("<C-return>" crux-smart-open-line-above :name "insert new line above" :exit t)
  ("SPC f"
   (("x" crux-create-scratch-buffer :name "scratch")
    ("r" crux-rename-file-and-buffer :name "rename")
    ("D" crux-delete-file-and-buffer :name "delete")))
  :hook
  (find-file . crux-reopen-as-root-mode))
;; crux:1 ends here

;; [[file:C4.org::*undo-fu][undo-fu:1]]
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
;; undo-fu:1 ends here

;; [[file:C4.org::*expand-region][expand-region:1]]
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
;; expand-region:1 ends here

;; [[file:C4.org::*selectrum][selectrum:1]]
;;; Better minibuffer completion
(use-package selectrum
  :config
  (selectrum-mode 1))
;; selectrum:1 ends here

;; [[file:C4.org::*prescient][prescient:1]]
;;; Remember frequently used commands and queries
(use-package selectrum-prescient
  :after selectrum
  :config
  (selectrum-prescient-mode 1)
  (prescient-persist-mode 1))
;; prescient:1 ends here

;; [[file:C4.org::*orderless][orderless:1]]
;;; Partial completion queries support
(use-package orderless
  :init
  (icomplete-mode)
  :custom
  (completion-styles '(orderless)))
;; orderless:1 ends here

;; [[file:C4.org::*consult][consult:1]]
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
;; consult:1 ends here

;; [[file:C4.org::*embark][embark:1]]
;;; An interface for minibuffer actions
(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))
;; embark:1 ends here

;; [[file:C4.org::*marginalia][marginalia:1]]
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
;; marginalia:1 ends here

;; [[file:C4.org::*ctrlf][ctrlf:1]]
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
;; ctrlf:1 ends here

;; [[file:C4.org::*User Settings][User Settings:1]]
;;; Set variables for my root project directory and GitHub username
(setq C4/project-root '("~/Code"))
(setq C4/gh-user "cr-jr")
;; User Settings:1 ends here

;; [[file:C4.org::*Management][Management:1]]
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
;; Management:1 ends here

;; [[file:C4.org::*magit][magit:1]]
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
;; magit:1 ends here

;; [[file:C4.org::*forge][forge:1]]
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
;; forge:1 ends here

;; [[file:C4.org::*diff-hl][diff-hl:1]]
  ;;; Show how files have changed between commits
  (use-package diff-hl
    :after magit
    :hook
    (magit-pre-refresh . diff-hl-magit-pre-refresh)
    (magit-post-refresh . diff-hl-magit-post-refresh)
    :config
    (global-diff-hl-mode 1))
;; diff-hl:1 ends here

;; [[file:C4.org::*langtool][langtool:1]]
;;; Writing improvement tools

;; Setup langtool
(use-package langtool
  :commands (langtool-check)
  :ryo
  (:mode 'text-mode)
  ("SPC d"
   (("d" langtool-check :name "check")
    ("D" langtool-check-done :name "done")
    ("i" langtool-show-message-at-point :name "info")
    ("c" langtool-correct-buffer :name "correct")) :name "writing assistant")
  :init
  (setq langtool-language-tool-server-jar "~/Source/LanguageTool-5.2-stable/languagetool-server.jar"))
;; langtool:1 ends here

;; [[file:C4.org::*mw-thesaurus][mw-thesaurus:1]]
;; Setup mw-thesaurus
(use-package mw-thesaurus
  :ryo
  (:mode 'text-mode)
  ("SPC d w" mw-thesaurus-lookup-dwim :name "word lookup")
  :custom
  (mw-thesaurus--api-key "629ccc6a-d13c-47dc-a3bd-4f807b3b90a6"))
;; mw-thesaurus:1 ends here

;; [[file:C4.org::*Setup][Setup:1]]
(defhydra org-trek (:timeout 30)
  "A transient mode to logically browse an Org file"
  ("h" org-forward-heading-same-level "jump to next heading (same level)")
  ("H" org-backward-heading-same-level "jump to prev heading (same level)")
  ("s" org-babel-next-src-block "jump to next src block")
  ("S" org-babel-previous-src-block "jump to prev src block")
  ("v" org-next-visible-heading "jump to next heading")
  ("V" org-previous-visible-heading "jump to prev heading")
  ("RET" nil "exit state: org-trek" :exit t))

(defhydra org-reorg (:timeout 30)
  "A transient mode to rearrange things"
  ("i" org-move-item-up "move item up")
  ("I" org-move-subtree-up "move subtree up")
  ("k" org-move-item-down "move item down")
  ("K" org-move-subtree-down "move subtree down")
  ("RET" nil "exit state: org-reorg" :exit t))


(defun ndk/heading-title ()
  "Get the heading title."
  (save-excursion
    (if (not (org-at-heading-p))
  (org-previous-visible-heading 1))
    (org-element-property :title (org-element-at-point))))

(defun ndk/org-breadcrumbs ()
  "Get the chain of headings from the top level down
    to the current heading."
  (let ((breadcrumbs (org-format-outline-path
                      (org-get-outline-path)
                      (1- (frame-width))
                      nil " ⟼ "))
        (title (ndk/heading-title)))
    (if (string-empty-p breadcrumbs)
        title
      (format "%s ⟼ %s" breadcrumbs title))))

(defun ndk/set-header-line-format()
  (setq header-line-format '(:eval (ndk/org-breadcrumbs))))


;;; Refiling setup
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Refile from current file
(defun my/org-refile-in-file (&optional prefix)
  "Refile to a target within the current file."
  (interactive)
  (let ((org-refile-targets `(((,(buffer-file-name)) :maxlevel . 6))))
    (call-interactively 'org-refile)))

;; Save all buffers after a refile
(advice-add 'org-refile :after 'org-save-all-org-buffers)


;;; Org setup
(use-package org
  :ryo
  ("SPC o" nil :name "org")
  (:mode 'org-mode)
  ("M b" org-babel-mark-block :name "block")
  ("M e" org-mark-element :name "element")
  ("SPC o a"
   (("a" org-agenda-list :name "weekly")
    ("f" org-agenda :name "full")
    ("t" org-set-tags-command :name "tags")) :name "agenda")
  ("SPC o b"
   (("b" org-insert-link :name "link")
    ("c" org-capture :name "capture")
    ("r" my/org-refile-in-file :name "refile")
    ("R" org-refile :name "to agenda")
    ("n"
     (("n" org-toggle-narrow-to-subtree :name "subtree")
      ("b" org-narrow-to-block :name "block")
      ("e" org-narrow-to-element :name "element")) :name "narrow")
    ("m" org-reorg/body :name "state: org-reorg")
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
  (org-mode . ndk/set-header-line-format)
  :custom-face
  (org-code ((t (:font "Cousine-14"))))
  (org-tag ((t (:inherit 'org-code))))
  (org-table ((t (:inherit 'org-code))))
  (org-verbatim ((t (:inherit 'org-code))))
  (org-ellipsis ((t (:underline nil))))
  (org-meta-line ((t (:inherit 'org-code :extend t))))
  (org-block ((t (:inherit 'fixed-pitch))))
  (org-block-begin-line ((t (:inherit 'fixed-pitch))))
  (org-block-end-line ((t (:inherit 'org-block-begin-line))))
  :config
  (setq org-ellipsis " ➕")
  (setq org-directory "~/Documents/Org/")
  (setq line-spacing 0.25)
  
  ;;; Org agenda flow
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  
  (setq org-agenda-files '("~/Documents/Org/Projects.org" "~/Documents/Org/Done.org"))
  
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence
           "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)"
           "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
  
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  
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
     (scheme . t) ;; TODO: figure out how to load Scheme with Geiser
     (C . t)
     (shell . t)
     (js . t)))
  )
;; Setup:1 ends here

;; [[file:C4.org::org-refiling][org-refiling]]
;;; Refiling setup
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Refile from current file
(defun my/org-refile-in-file (&optional prefix)
  "Refile to a target within the current file."
  (interactive)
  (let ((org-refile-targets `(((,(buffer-file-name)) :maxlevel . 6))))
    (call-interactively 'org-refile)))

;; Save all buffers after a refile
(advice-add 'org-refile :after 'org-save-all-org-buffers)
;; org-refiling ends here

;; [[file:C4.org::*org-superstar][org-superstar:1]]
;;; Org Superstar makes your bullets bang louder
(use-package org-superstar
  :after org
  :hook
  (org-mode . org-superstar-mode)
  :custom-face
  (org-superstar-leading ((t (:inherit 'org-hide))))
  :init
  (setq org-superstar-headline-bullets-list
        '("§" "☙" "჻")))
;; org-superstar:1 ends here

;; [[file:C4.org::*visual-fill-column][visual-fill-column:1]]
;;; Org mode line length
(defvar C4/org-measure 140)

;;; visual-fill-column does just enough UI adjustment for Org Mode
(use-package visual-fill-column
  :custom
  (visual-fill-column-width C4/org-measure)
  (visual-fill-column-center-text t)
  :hook
  (visual-line-mode . visual-fill-column-mode)
  (org-mode . (lambda () (setq fill-column C4/org-measure)))
  :config
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))
;; visual-fill-column:1 ends here

;; [[file:C4.org::*toc-org][toc-org:1]]
;;; Add support for a table of contents
(use-package toc-org
  :after org
  :hook
  (org-mode . toc-org-mode))
;; toc-org:1 ends here

;; [[file:C4.org::*Journaling][Journaling:1]]
;;; Journal file header
(defun C4/org-journal-file-header (time)
  "Custom function to create a journal header."
  (concat
   (pcase org-journal-file-type
     (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything\n")
     (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded\n")
     (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded\n")
     (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded\n"))))

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
  (org-journal-dir "~/Documents/Org/Notes/Journal/")
  (org-journal-file-format "%V|%F.org")

  ;; Entries
  (org-journal-file-header 'C4/org-journal-file-header)

  ;; Org agenda integration
  (org-journal-enable-agenda-integration t))
;; Journaling:1 ends here

;; [[file:C4.org::*org-roam][org-roam:1]]
;;; Setup org-roam for starting a knowledge base
(use-package org-roam
  :after org
  :ryo
  (:mode 'org-roam-mode)
  ("SPC o n"
   (("n" org-roam-buffer-toggle-display :name "toggle")
    ("b" org-roam-switch-to-buffer :name "switch")
    ("f" org-roam-find-file :name "find file")
    ("g" org-roam-graph :name "graph")
    ("l" org-roam-insert :name "link")
    ("L" org-roam-insert-immediate :name "and create note")) :name "roam")
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/Documents/Org/Notes/Roam/")
  :init
  (setq org-roam-v2-ack t))
;; org-roam:1 ends here

;; [[file:C4.org::*deft][deft:1]]
;;; Deft for quick pattern-based note searching
(use-package deft
  :ryo
  ("SPC o q" deft :name "query")
  :commands (deft)
  :custom
  (deft-extensions '("org"))
  (deft-directory "~/Documents/Org/Notes/")
  (deft-use-filename-as-title t)
  (deft-recursive t))
;; deft:1 ends here

;; [[file:C4.org::*weblorg][weblorg:1]]
;;; An Org Mode static site generator
(use-package weblorg)
;; weblorg:1 ends here

;; [[file:C4.org::*asdf-vm][asdf-vm:1]]
(use-package exec-path-from-shell
  :if window-system
  :config (exec-path-from-shell-initialize))
;; asdf-vm:1 ends here

;; [[file:C4.org::*Parsing][Parsing:1]]
;;; A full on parser in Emacs with highlighting definitions
(use-package tree-sitter
  :config
  (global-tree-sitter-mode 1))

;; A collection of supported tree-sitter languages
(use-package tree-sitter-langs
  :after tree-sitter)
;; Parsing:1 ends here

;; [[file:C4.org::*Faces][Faces:1]]
;;; Set syntax highlighting faces

;; set comment face
(set-face-attribute 'font-lock-comment-face nil
        :font "Cousine:italic"
        :foreground (ewal-load-color 'white))

;; set keyword face
(set-face-attribute 'font-lock-keyword-face nil
        :font "Cousine Bold" :foreground (ewal-load-color 'white))

;; set function name face
(set-face-attribute 'font-lock-function-name-face nil
        :font "Cousine" :weight 'bold :foreground (ewal-load-color 'yellow))

;; set string face
(set-face-attribute 'font-lock-string-face nil
        :font "Cousine:italic" :foreground (ewal-load-color 'green))

;; set docstring face
(set-face-attribute 'font-lock-doc-face nil :font "Cousine")

;; set constants face
(set-face-attribute 'font-lock-constant-face nil :inherit 'font-lock-function-name-face)

;; set built-in face
(set-face-attribute 'font-lock-builtin-face nil :inherit 'font-lock-keyword-face)

;; set variable name face
(set-face-attribute 'font-lock-variable-name-face nil :inherit 'font-lock-function-name-face)
;; Faces:1 ends here

;; [[file:C4.org::*rainbow-delimiters][rainbow-delimiters:1]]
;;; When I'm knee deep in parens
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (prog-mode . prettify-symbols-mode))
;; rainbow-delimiters:1 ends here

;; [[file:C4.org::*color-identifiers-mode][color-identifiers-mode:1]]
;;; Helps me remember the names of things
(use-package color-identifiers-mode
  :hook
  (prog-mode . color-identifiers-mode))
;; color-identifiers-mode:1 ends here

;; [[file:C4.org::*Linting][Linting:1]]
;;; Code linting package that flies
(use-package flycheck
  :hook (prog-mode . flycheck-mode))
;; Linting:1 ends here

;; [[file:C4.org::*apheleia][apheleia:1]]
;;; Universal code formatting package
(use-package apheleia
  :straight
  '(apheleia
    :host github
    :repo "raxod502/apheleia")
  :hook (prog-mode . apheleia-mode))
;; apheleia:1 ends here

;; [[file:C4.org::*smartparens][smartparens:1]]
;;; Autopair delimiters
(use-package smartparens
  :hook
  (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))
;; smartparens:1 ends here

;; [[file:C4.org::*aggressive-indent-mode][aggressive-indent-mode:1]]
;;; Automatic indentation for my sanity
(use-package aggressive-indent
  :hook
  (prog-mode . aggressive-indent-mode))
;; aggressive-indent-mode:1 ends here

;; [[file:C4.org::*Language Server Protocol][Language Server Protocol:1]]
;;; Language Server Protocol package for rich IDE features

;; Setup eglot: a lightweight LSP client
(use-package eglot
  :ryo
  (:mode 'eglot--managed-mode)
  ("SPC l l"
   (("c" eglot :name "connect")
    ("C" eglot-reconnect :name "restart")
    ("C-c" eglot-shutdown :name "shutdown")
    ("e"
     (("e" eglot-events-buffer :name "show events")
      ("E" eglot-stderr-buffer :name "show errors")
      ("c" eglot-signal-didChangeConfiguration :name "reload workspace config")) :name "client actions")
    ("a" eglot-code-actions :name "code actions")
    ("r" eglot-rename :name "rename symbol")
    ("f" eglot-format :name "format")
    ("d" eldoc :name "documentation")) :name "LSP"))
;; Language Server Protocol:1 ends here

;; [[file:C4.org::*Snippets][Snippets:1]]
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
  (:mode 'html-mode)
  ("SPC s"
   (("s" aya-create :name "create")
    ("e" C4/expand-snippet :name "expand" :exit t)
    ("w" C4/save-snippet :name "save")) :name "snippet"))
;; Snippets:1 ends here

;; [[file:C4.org::*Emacs Lisp][Emacs Lisp:1]]
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
;; Emacs Lisp:1 ends here

;; [[file:C4.org::*Common Lisp][Common Lisp:1]]
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
  (setq inferior-lisp-program "/home/cr-jr/.guix-extra-profiles/work/work/bin/sbcl")

  (sly))
;; Common Lisp:1 ends here

;; [[file:C4.org::*Racket][Racket:1]]
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
  (add-to-list 'org-babel-load-languages '(racket . t))
  (add-to-list 'org-babel-load-languages '(scribble . t)))
;; Racket:1 ends here

;; [[file:C4.org::*Guile][Guile:1]]
;;; Lang: Guile

(use-package geiser-guile
  :after geiser
  :mode ("\\.scm\\'" . scheme-mode)
  :interpreter ("guile" . scheme-mode)
  :hook
  (scheme-mode . geiser-mode)
  :config
  (setq geiser-default-implementation 'guile))

(use-package macrostep-geiser :after geiser)
;; Guile:1 ends here

;; [[file:C4.org::*Web Dev][Web Dev:1]]
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
(use-package ob-html
  :straight (ob-html :host github :repo "misohena/ob-html")
  :after org
  :config
  (setq org-babel-html-chrome-executable "/home/cr-jr/.guix-profile/bin/chromium")
  (append '((html . t)) org-babel-load-languages))
;; Web Dev:1 ends here

;; [[file:C4.org::*Setup][Setup:1]]
;;; Lang: JavaScript

;; Setup js2-mode and use it to augment the built-in mode
(use-package js2-mode
  :mode ("\\.js\\'" . js-mode)
  :interpreter ("deno" . js-mode)
  :ryo
  (:mode 'js-mode)
  ("SPC l" nil :name "javascript")
  :hook
  (js-mode . js2-minor-mode)
  (js-mode . eglot-ensure)
  :config
  ;; Setup deno built-in LSP for eglot
  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for handling Deno's built-in LSP server")

  ;; Deno requires the :enable keyword to connect, but I also want to include
  ;; the built-in linting and begin with good habits since I'm new to the space.
  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options."
    (let* ((root (car (project-roots (eglot--project server))))
     (cache (expand-file-name ".deno/lsp/cache/" root)))
      (list :enable t :lint t)))

  ;; Note: The deno lsp JavaScript language identifier is NOT "js", so eglot's
  ;; guess ("js" for js-mode) was incorrect and the server wouldn't load
  (add-to-list
   'eglot-server-programs '((js-mode :language-id "javascript") . (eglot-deno "deno" "lsp")))

  (add-hook
   'js-mode-hook (lambda () (add-hook 'before-save-hook 'eglot-format-buffer))))

;; Setup typescript-mode
(use-package typescript-mode
  :after js2-mode
  :mode ("\\.ts\\'" . typescript-mode)
  :interpreter ("deno" . typescript-mode)
  :ryo
  (:mode 'typescript-mode)
  ("SPC l" nil :name "typescript")
  :hook
  (typescript-mode . eglot-ensure)
  :config
  (add-to-list
   'eglot-server-programs '(typescript-mode . (eglot-deno "deno" "lsp")))

  (add-hook
   'typescript-mode-hook (lambda () (add-hook 'before-save-hook 'eglot-format-buffer))))

;; Support literate programming with TypeScript
(use-package ob-typescript
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(typescript . t)))

;; Literate programming with the deno runtime
(use-package ob-deno
  :after org
  :straight '(ob-deno :host github :repo "cr-jr/ob-deno")
  :config
  (add-to-list 'org-babel-load-languages '(deno . t))
  (add-to-list 'org-src-lang-modes '("deno" . js))
  (add-to-list 'org-src-lang-modes '("deno" . typescript)))

;; Setup json-mode
(use-package json-mode
  :mode
  ("\\.json\\'" . json-mode)
  ("\\.jsonp\\'" . json-mode))
;; Setup:1 ends here

;; [[file:C4.org::*rainbow-mode][rainbow-mode:1]]
(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode))
;; rainbow-mode:1 ends here

;; [[file:C4.org::*Guix][Guix:1]]
;;; Guix

(use-package guix
  :hook
  (scheme-mode . guix-devel-mode)
  :config
  (with-eval-after-load 'geiser-guile
    (add-to-list 'geiser-guile-load-path "~/Code/guix")))
;; Guix:1 ends here
