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
    :config (projectile-mode)
    :bind-keymap
    ("C-c p" . projectile-command-map)
    :init
    (when (file-directory-p root-path)
      (setq projectile-project-search-path (list root-path)))
    (setq projectile-switch-project-action #'projectile-dired))

  (use-package counsel-projectile
    :after projectile))

(defun c4/git ()
  (use-package magit))

(provide 'projects)
;;;; projects.el ends here
