;;;; C4 Projects
;;;;
;;;; Commentary:
;;;;
;;;; This file contains modules for configuring the way I work with
;;;; my projects, code or otherwise.
;;;;
;;;; Code:

(defun c4/projects (root-path)
  (use-package projectile
    :diminish
    :config
    (projectile-mode)
    :custom
    (projectile-completion-system 'ivy)
    :bind-keymap
    ("C-c p" . projectile-command-map)
    :init
    (when (file-directory-p root-path)
      (setq projectile-project-search-path (list root-path)))
    (setq projectile-switch-project-action #'projectile-dired))

  (use-package counsel-projectile
    :after projectile
    :config
    (counsel-projectile-mode)))

(defun c4/git (user)
  (use-package magit
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
    :config
    (setq auth-sources '("~/.authinfo"))
    (ghub-request "GET" "/user" nil
      :forge 'github
      :host "api.github.com"
      :username user
      :auth 'forge)))

(provide 'projects)
;;;; projects.el ends here
