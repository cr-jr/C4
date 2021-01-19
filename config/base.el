;;;; C4 Base
;;;;
;;;; Commentary:
;;;;
;;;; This file contains the base, essential settings for
;;;; my config that must load if nothing else does.
;;;;
;;;; Code:

;;; Early init settings
(defun c4/init ()
  "Loaded and ran in early-init.el."
  (progn
    (c4/perf)
    (c4/package-init)
    (c4/housekeeping)))

(defun c4/package-init ()
  "Initializes package package management with straight.el & use-package."
  ;; Initialize straight.el for package management
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
  (setq straight-use-package-by-default t))

(defun c4/perf ()
  "Handles performance optimizations"

  ;; Raise the garbage collection threshold high as emacs starts
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))

  ;; Drop it down once loaded
  (add-hook
  'after-init-hook
  #'(lambda () (setq gc-cons-threshold 80000))))

(defun c4/housekeeping ()
  "A helper for should-be-defaults"
  ;; Lockfiles do more harm than good
  (setq create-lockfiles nil)

  ;; Custom files just add clutter
  (setq custom-file null-device)

  (add-hook 'before-save-hook
    'delete-trailing-whitespace)    ; Delete trailing whitespace on save

  ;; Create parent dirs when opening new files
  (add-to-list 'find-file-not-found-functions #'c4/create-parent-on-file-find)

  ;; I really don't like clutter. Really :P
  (use-package no-littering
  :init
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))))

(defun c4/create-parent-on-file-find ()
  "Ensures that the parent dirs are created for a nonexistent file."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
	       (y-or-n-p (format
			  "Directory `%s' does not exist! Create it?"
			  parent-directory)))
      (make-directory parent-directory t))))

(defun c4/user (name email)
  "Define user identity."
  (setq user-full-name name
	user-full-email email))

;;; UI base
(defun c4/base ()
  (progn
    (c4/ui)
    (c4/ux)
    (c4/keybindings)
    (c4/programming)))

(defun c4/ui ()
  "A module for the base UI."
  (setq-default cursor-type 'bar) ; default cursor as bar
  (setq-default frame-title-format '("%b")) ; window title is the buffer name

  (setq linum-format "%4d ") ; line number format
  (column-number-mode 1)
  (show-paren-mode 1) ; show closing parens by default

  (menu-bar-mode -1) ; disable the menubar
  (scroll-bar-mode -1) ; disable visible scroll bar
  (tool-bar-mode -1) ; disable toolbar
  (tooltip-mode -1) ; disable tooltips
  (set-fringe-mode 16) ; allow some space

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

  ;; Tidy up the modeline
  (use-package diminish)
  ;; And let's make it a bit sexier
  (use-package all-the-icons))

;;; UI settings
(defun c4/theming (theme)
  "A module for setting a theme and configuring the modeline."
  (setq theme-sym (symbol-name theme))

  (use-package smart-mode-line
    :init
    (setq sml/theme 'respectful)
    (setq sml/no-confirm-load-theme t)
    (setq sml/name-width 64)
    (setq sml/mode-width 'full)
    :config
    (sml/setup)
    (add-to-list 'sml/replacer-regexp-list '("^~/.config/emacs/" ":C4:") t)
    (add-to-list 'sml/replacer-regexp-list '("^~/Workbench/" ":Projects:") t)
    (add-to-list 'sml/replacer-regexp-list '("^~/Org/" ":Org:") t))

  (cond ((string-prefix-p "modus" theme-sym)
	 (use-package modus-themes
	   :config
	   (load-theme theme t)))
  ((string-prefix-p "minimal" theme-sym)
    (use-package minimal-theme
      :config (load-theme theme t)))
	((string-prefix-p "doom" theme-sym)
	 (use-package doom-themes
	   :config (load-theme theme t)))
	(t (load-theme theme t))))

(defun c4/typography (family &optional size)
  "A module for setting typography."
  (set-face-attribute 'default nil
    :font (format "%s-%s:slant=normal" family (or size 12)))
  (set-face-attribute 'italic nil
    :font (format "%s-%s:slant=italic" family (or size 12)))
  (set-face-attribute 'bold nil
    :font (format "%s-%s:weight=bold" family (or size 12)))
  (set-face-attribute 'bold-italic nil
    :font (format "%s-%s:weight=bold:slant=italic" family (or size 12))))

;;; UX base
(defun c4/ux ()
  "A module for the 'base' UX."
  (setq inhibit-startup-message t) ; inhibit startup message
  (setq initial-scratch-message "") ; no scratch message
  (setq visible-bell t)             ; enable visual bell
  (global-auto-revert-mode t) ; autosave buffer on file change
  (delete-selection-mode 1) ; Selected text will be overwritten on typing
  (fset 'yes-or-no-p 'y-or-n-p) ; convert "yes" or "no" confirms to "y" and "n"

  ;; See a database of all defined keybindings
  (use-package which-key
    :init
    (setq which-key-idle-delay 0.96)
    :diminish
    :config
    (which-key-mode))

  ;; Better help documentation
  (use-package helpful
    :custom
    (counsel-describe-function-function #'helpful-callable)
    (counsel-describe-variable-function #'helpful-variable)
    :bind
    ([remap describe-function] . counsel-describe-function)
    ([remap describe-command] . helpful-command)
    ([remap describe-variable] . counsel-describe-variable)
    ([remap describe-key] . helpful-key))

  ;; Editorconfig
  (use-package editorconfig
    :diminish
    :config
    (editorconfig-mode))

  ;; Better terminal
  (use-package vterm)

  (c4/lookup))

(defun c4/lookup ()
  "A module for search functionality."
  ;; Incremental search
  (use-package swiper)

  ;; Lookup enhancements
  (use-package ivy
    :diminish
    :init
    (setq ivy-initial-inputs-alist nil) ; no ^ before searches
    :bind
    (("C-s" . swiper)
    ("C-r" . swiper-backward)
    :map ivy-minibuffer-map
    ("TAB" . ivy-alt-done)
    ("C-l" . ivy-alt-done)
    ("C-j" . ivy-next-line)
    ("C-k" . ivy-previous-line)
    :map ivy-switch-buffer-map
    ("C-k" . ivy-previous-line)
    ("C-l" . ivy-done)
    ("C-d" . ivy-switch-buffer-kill)
    :map ivy-reverse-i-search-map
    ("C-k" . ivy-previous-line)
    ("C-d" . ivy-reverse-i-search-kill)))

  (use-package counsel
    :diminish
    :bind
    (("C-x b" . counsel-ibuffer)
    ("C-M-j" . counsel-switch-buffer)
    :map minibuffer-local-map
    ("C-x r" . 'counsel-minibuffer-history))
    :config
    (counsel-mode 1))

  (use-package ivy-rich
    :after (ivy counsel)
    :config
    (ivy-rich-mode))

  ;; Command sorting recent
  (use-package ivy-prescient
    :after counsel
    :custom
    (ivy-prescient-enable-filtering nil)
    :config
    (prescient-persist-mode 1)
    (ivy-prescient-mode 1)))

;;; Programming base
(defun c4/programming ()
  (use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode)))

;;; Keybindings

(defun c4/keybindings ()
  (c4/evil)
  (c4/transient))

(defun c4/evil ()
  (use-package evil
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-i-jump nil)
    :hook
    (emacs-startup . evil-mode)
    :config
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
    (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

    ;; Use visual line motions even outside of visual-line-mode buffers
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
    (evil-set-initial-state 'messages-buffer-mode 'normal)

    (evil-set-initial-state 'dashboard-mode 'normal))

  (use-package evil-collection
    :after evil
    :config
    (evil-collection-init))

  ;; Leader setup
  (use-package general
    :after evil
    :config
    (general-evil-setup 1)

    (general-create-definer c4/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix [\s-SPC])

    (c4/mnemonics)))

(defun c4/transient ()
  (use-package hydra
    :config
    (defhydra hydra-text-scale (:timeout 4)
      "scale text"
      ("j" text-scale-increase "in")
      ("k" text-scale-decrease "out")
      ("f" nil "finished" :exit t))))

(defun c4/mnemonics ()
  (c4/leader-key-def
    ;; Global
    "'" '(vterm :which-key "open terminal")
    "u" '(universal-argument :which-key "command modifier")

    ;; Buffer
    "b" '(:ignore t :which-key "buffer")
    "bb" '(counsel-switch-buffer :which-key "switch")
    "bd" '(kill-current-buffer :which-key "kill")
    "bD" '(kill-some-buffers :which-key "kill multiple")
    "bk" '(kill-current-buffer :which-key "kill")
    "bK" '(kill-some-buffers :which-key "kill multiple")
    "bw" '(save-buffer :which-key "write")
    "bW" '(save-some-buffers :which-key "write modified")

    ;; C4 config
    "c" '(:ignore t :which-key "C4 config")
    "cc" '(c4/find-config :which-key "find")
    "cr" '(c4/reload-config :which-key "reload")
    "ce" '(:ignore t :which-key "eval")
    "cee" '(eval-last-sexp :which-key "S-exp")
    "ceb" '(eval-buffer :which-key "buffer")
    "cer" '(eval-region :which-key "region")

    ;; Files
    "f" '(:ignore t :which-key "file")
    "ff" '(counsel-find-file :which-key "find")

    ;; Help
    "h" '(:ignore t :which-key "help")
    "hd" '(:ignore t :which-key "describe")
    "hdf" '(counsel-describe-function :which-key "function")
    "hdF" '(counsel-describe-face :which-key "face")
    "hdc" '(helpful-command :which-key "command")
    "hdv" '(counsel-describe-variable :which-key "variable")
    "hdk" '(helpful-key :which-key "keybinding")
    "hm" '(:ignore t :which-key "manual")
    "hmm" '(info-emacs-manual :which-key "emacs")

    ;; Projects
    "p" '(:ignore t :which-key "project")
    "p'" '(projectile-run-vterm :which-key "open terminal")
    "pp" '(counsel-projectile-switch-project :which-key "switch")
    "pf" '(counsel-projectile-find-file :which-key "find file")
    "pg" '(:ignore t :which-key "git")
    "pgs" '(magit-status :which-key "status")

    ;; Session
    "q" '(:ignore t :which-key "quit")
    "qq" '(save-buffers-kill-emacs :which-key "and save")
    "qQ" '(kill-emacs :which-key "really quit")

    ;; Search
    "s" '(:ignore t :which-key "search")
    "sb" '(swiper :which-key "buffer")
    "sp" '(counsel-projectile-rg :which-key "project")

    ;; Toggle
    "t" '(:ignore t :which-key "toggle")
    "tu" '(:ignore t :which-key "ui")
    "tuk" '(exwm-layout-toggle-keyboard :which-key "keyboard")
    "tum" '(exwm-layout-toggle-mode-line :which-key "mode line")
    "tuM" '(exwm-layout-toggle-minibuffer :which-key "minibuffer")
    "tut" '(counsel-load-theme :which-key "theme")
    "tus" '(hydra-text-scale/body :which-key "scale text")

    ;; Window
    "w" '(:ignore t :which-key "window")
    "wc" '(evil-window-delete :which-key "close")
    "wf" '(exwm-floating-toggle-floating :which-key "floating")
    "wF" '(exwm-layout-toggle-fullscreen :which-key "fullscreen")
    "ws" '(:ignore t :which-key "split")
    "wss" '(evil-window-split :which-key "horizontal")
    "wsS" '(evil-window-vsplit :which-key "vertical")))

(defun c4/find-config ()
  "Open files in config directory."
  (interactive)
  (counsel-find-file nil "~/.config/emacs/config/"))

(defun c4/reload-config ()
  "Reloads the config in place."
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

(provide 'base)
;;;; base.el ends here
