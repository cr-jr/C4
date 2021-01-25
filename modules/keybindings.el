(defun c4/keybindings ()
  (c4/evil)
  (c4/transient)
  (c4/mnemonics))

(defun c4/evil ()
  (use-package evil
    :custom
    (evil-want-integration t)
    (evil-want-keybinding nil)
    (evil-want-C-u-scroll t)
    (evil-want-C-i-jump nil)
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
    (evil-collection-init)))

(defun c4/transient ()
  (use-package hydra
    :config
    (defhydra hydra-org-src-find (:timeout 3)
      "cycle through all source blocks in the current buffer"
      ("j" org-babel-next-src-block "next")
      ("k" org-babel-previous-src-block "previous")
      ("RET" nil "exit" :exit t))

    (defhydra hydra-org-heading-find (:timeout 3)
      "cycle through all headings at the current level"
      ("j" org-forward-heading-same-level "next")
      ("k" org-backward-heading-same-level "previous")
      ("RET" nil "exit" :exit t))

    (defhydra hydra-text-scale (:timeout 4)
      "scale text"
      ("j" text-scale-increase "in")
      ("k" text-scale-decrease "out")
      ("RET" nil "exit" :exit t))))

(defun c4/mnemonics ()
  (use-package general
    :after evil
    :config
    (general-evil-setup 1)
    (general-create-definer c4/leader-key-def
      :keymaps '(normal insert visual emacs)
      :prefix "SPC"
      :global-prefix [\s-SPC])

    (c4/key-def-global)
    (c4/key-def-buffer)
    (c4/key-def-config)
    (c4/key-def-file)
    (c4/key-def-help)
    (c4/key-def-org)
    (c4/key-def-project)
    (c4/key-def-session)
    (c4/key-def-toggle)
    (c4/key-def-window)))

(defun c4/key-def-global ()
  (c4/leader-key-def
    "'" '(vterm :which-key "open terminal")
    "SPC" '(universal-argument :which-key "command modifier")))

(defun c4/key-def-buffer ()
  (c4/leader-key-def
    "b" '(:ignore t :which-key "buffer")
    "bb" '(counsel-switch-buffer :which-key "switch")
    "bd" '(kill-current-buffer :which-key "kill")
    "bD" '(kill-some-buffers :which-key "kill multiple")
    "bn" '(:ignore t :which-key "narrow")
    "bnn" '(widen :which-key "reset")
    "bnd" '(narrow-to-defun :which-key "to defun")
    "bnp" '(narrow-to-page :which-key "to page")
    "bnr" '(narrow-to-region :which-key "to region")
    "bk" '(kill-current-buffer :which-key "kill")
    "bK" '(kill-some-buffers :which-key "kill multiple")
    "bs" '(swiper :which-key "search")
    "bS" '(swiper-backward :which-key "search backward")
    "bw" '(save-buffer :which-key "write")
    "bW" '(save-some-buffers :which-key "write modified")))

(defun c4/key-def-config ()
  (c4/leader-key-def
    "c" '(:ignore t :which-key "C4 config")
    "cc" '(c4/open-config :which-key "open")
    "cd" '(:ignore t :which-key "debug")
    "cdd" '(c4/esup-init :which-key "startup")
    "cde" '(c4/bug-hunter-init :which-key "errors")
    "cdp" '(explain-pause-top :which-key "processes")
    "cr" '(c4/reload-config :which-key "reload")
    "ce" '(:ignore t :which-key "eval")
    "cee" '(eval-last-sexp :which-key "S-exp")
    "ced" '(eval-defun :which-key "defun")
    "cer" '(eval-region :which-key "region")))

(defun c4/esup-init ()
  "Profiles the correct init file"
  (interactive)
  (esup (concat user-emacs-directory "init.el")))

(defun c4/bug-hunter-init ()
  "Debugs the correct init file"
  (interactive)
  (bug-hunter-file (concat user-emacs-directory "C4.el")))

(defun c4/open-config ()
  "Open files in config directory."
  (interactive)
  (counsel-find-file nil user-emacs-directory))

(defun c4/reload-config ()
  "Reloads the config in place."
  (interactive)
  (load-file (concat user-emacs-directory "C4.el")))

(defun c4/key-def-file ()
  (c4/leader-key-def
    "f" '(:ignore t :which-key "file")
    "ff" '(counsel-find-file :which-key "find")))

(defun c4/key-def-help ()
  (c4/leader-key-def
    "h" '(:ignore t :which-key "help")
    "ha" '(counsel-apropos :which-key "apropos")
    "hd" '(:ignore t :which-key "describe")
    "hdd" '(counsel-describe-function :which-key "function")
    "hdf" '(counsel-describe-face :which-key "face")
    "hdc" '(helpful-command :which-key "command")
    "hdv" '(counsel-describe-variable :which-key "variable")
    "hdk" '(helpful-key :which-key "keybinding")
    "hds" '(helpful-at-point :which-key "symbol at point")
    "hm" '(:ignore t :which-key "manual")
    "hmm" '(info-emacs-manual :which-key "emacs")))

(defun c4/key-def-org ()
  (c4/leader-key-def
    "o" '(:ignore t :which-key "org")
    "oa" '(:ignore t :which-key "agenda")
    "oaa" '(org-agenda-list :which-key "weekly")
    "oaf" '(org-agenda :which-key "full")
    "oat" '(org-set-tags-command :which-key "tags")
    "ob" '(:ignore t :which-key "buffer")
    "obb" '(org-insert-link :which-key "link")
    "obc" '(org-capture :which-key "capture")
    "obn" '(:ignore t :which-key "narrow")
    "obnn" '(org-toggle-narrow-to-subtree :which-key "subtree")
    "obnb" '(org-narrow-to-block :which-key "block")
    "obne" '(org-narrow-to-element :which-key "element")
    "obr" '(org-refile :which-key "refile")
    "obs" '(:ignore t :which-key "search")
    "obss" '(hydra-org-src-find/body :which-key "src blocks")
    "obsh" '(hydra-org-heading-find/body :which-key "headings")
    "od" '(:ignore t :which-key "date")
    "odd" '(org-deadline :which-key "deadline")
    "ods" '(org-schedule :which-key "schedule")
    "os" '(:ignore t :which-key "source")
    "ose" '(org-edit-special :which-key "edit")
    "osw" '(org-edit-src-save :which-key "save edits")
    "oss" '(org-babel-execute-src-block :which-key "execute source")
    "ost" '(org-babel-tangle :which-key "tangle")))

(defun c4/key-def-project ()
  (c4/leader-key-def
    "p" '(:ignore t :which-key "project")
    "p'" '(projectile-run-vterm :which-key "open terminal")
    "pp" '(counsel-projectile-switch-project :which-key "switch")
    "pf" '(counsel-projectile-find-file :which-key "find file")
    "pg" '(:ignore t :which-key "git")
    "pgg" '(magit-status :which-key "status")
    "pgc" '(magit-commit :which-key "commit")
    "pgd" '(magit-diff :which-key "diff")
    "pgf" '(:ignore t :which-key "forge")
    "pgff" '(forge-pull :which-key "pull")
    "pgfF" '(forge-fork :which-key "fork repo")
    "pgfi" '(forge-list-issues :which-key "issues")
    "pgfI" '(forge-create-issue :which-key "create issue")
    "pgi" '(magit-init :which-key "init")
    "pgp" '(magit-push :which-key "push")
    "pgP" '(magit-pull :which-key "pull")
    "pgr" '(magit-remote :which-key "remote")
    "pgs" '(magit-stage :which-key "stage")
    "pgS" '(magit-stage-file :which-key "stage file")
    "ps" '(counsel-projectile-rg :which-key "search")))

(defun c4/key-def-session ()
  (c4/leader-key-def
    "q" '(:ignore t :which-key "quit")
    "qq" '(save-buffers-kill-emacs :which-key "and save")
    "qQ" '(kill-emacs :which-key "really quit")))

(defun c4/key-def-toggle ()
  (c4/leader-key-def
    "t" '(:ignore t :which-key "toggle")
    "tt" '(counsel-load-theme :which-key "theme")
    "ts" '(hydra-text-scale/body :which-key "scale text")))

(defun c4/key-def-window ()
  (c4/leader-key-def
  "w" '(:ignore t :which-key "window")
  "wc" '(evil-window-delete :which-key "close")
  "wd" '(:ignore t :which-key "desktop")
  "wdf" '(exwm-floating-toggle-floating :which-key "floating")
  "wdF" '(exwm-layout-toggle-fullscreen :which-key "fullscreen")
  "wdk" '(exwm-layout-toggle-keyboard :which-key "keyboard mode")
  "wdm" '(exwm-layout-toggle-mode-line :which-key "mode line")
  "wdM" '(exwm-layout-toggle-minibuffer :which-key "minibuffer")
  "ws" '(:ignore t :which-key "split")
  "wss" '(evil-window-split :which-key "horizontal")
  "wsS" '(evil-window-vsplit :which-key "vertical")))

(provide 'keybindings)
