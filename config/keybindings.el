;;;; C4 Keybindings
;;;;
;;;; Commentary:
;;;;
;;;; This file contains the keybindings, based on modal editing and Vim
;;;; emulation used throughout this configuration.
;;;;
;;;; Code:

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
    "pgc" '(magit-commit :which-key "commit")
    "pgd" '(magit-diff :which-key "diff")
    "pgf" '(:ignore t :which-key "forge")
    "pgff" '(forge-pull :which-key "pull")
    "pgfF" '(forge-fork :which-key "fork repo")
    "pgfi" '(forge-list-issues :which-key "issues")
    "pgfI" '(forge-create-issue :which-key "create issue")
    "pgg" '(magit-status :which-key "status")
    "pgi" '(magit-init :which-key "init")
    "pgp" '(magit-push :which-key "push")
    "pgP" '(magit-pull :which-key "pull")
    "pgr" '(magit-remote :which-key "remote")
    "pgs" '(magit-stage :which-key "stage")
    "pgS" '(magit-stage-file :which-key "stage file")

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

(provide 'keybindings)
;;;; keybindings.el ends here
(provide 'keybindings)
;;;; keybindings.el ends here
