;;;; C4 Base
;;;;
;;;; Commentary:
;;;;
;;;; This file contains the base, essential settings for
;;;; my config that must load if nothing else does.
;;;;
;;;; Code:

;;; User settings
(cl-defun c4/user (&key name email)
  "Define user identity."
  (setq user-full-name name
	user-full-email email))

(cl-defun c4/base (&key theme typography)
  (eval (c4/ui theme typography))
  (c4/ux))

;;; UI base
(defun c4/ui (theme typography)
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
  (use-package all-the-icons)

  (eval (c4/theme theme))
  (eval typography))

;;; UI settings
(defun c4/theme (theme)
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

(cl-defun c4/typography (&key fixed variable)
  "A module for setting typography."
  (set-face-attribute 'default nil
    :font (format "%s-%s:slant=normal" (car fixed) (cadr fixed)))
  (set-face-attribute 'fixed-pitch nil
    :font (format "%s-%s" (car fixed) (cadr fixed)))
  (set-face-attribute 'variable-pitch nil
    :font (format "%s-%s" (car variable) (cadr variable))))

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

(provide 'base)
;;;; base.el ends here
