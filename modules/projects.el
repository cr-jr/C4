(cl-defun c4/projects (&key path username)
  (use-package projectile
    :diminish
    :config
    (projectile-mode)
    :custom
    (projectile-project-search-path (list path))
    (projectile-completion-system 'ivy)
    (projectile-switch-project-action #'projectile-dired)
    :bind-keymap
    ("C-c p" . projectile-command-map))

  (use-package counsel-projectile
    :after projectile
    :config
    (counsel-projectile-mode))
  (eval (c4/git username)))

(defun c4/git (user)
  (use-package magit
    :commands (magit magit-status)
    :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

  (use-package diff-hl
    :after magit
    :hook
    (after-init . global-diff-hl-mode)
    (magit-pre-refresh . diff-hl-magit-pre-refresh)
    (magit-post-refresh . diff-hl-magit-post-refresh))

  (use-package forge
    :after magit
    :custom
    (auth-sources '("~/.authinfo"))
    :config
    (ghub-request "GET" "/user" nil
      :forge 'github
      :host "api.github.com"
      :username user
      :auth 'forge)))

(provide 'projects)
