;;;; C4 Org-mode
;;;;
;;;; Commentary:
;;;;
;;;; This file contains functionality that configures Org-mode and its attendant
;;;; packages for this configuration.
;;;;
;;;; Code:

(cl-defun c4/org (&key path)
  (use-package org
    :straight org-plus-contrib
    :custom
    (org-ellipsis " ↴")
    (org-agenda-start-with-log-mode t)
    (org-log-done 'time)
    (org-log-into-drawer t)
    (org-agenda-files
      '("~/.config/emacs/sample/tasks.org"))
    (org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
         (sequence
           "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)"
           "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
    :hook
    (org-mode . c4/org-init))

  (use-package visual-fill-column
    :defer t
    :custom
    (visual-fill-column-width 120)
    (visual-fill-column-center-text t)
    :hook (org-mode . visual-fill-column-mode))

  (use-package org-bullets
    :after org
    :hook
    (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list
      '("§" "☙" "჻" " " " " " " " "))))

(defun c4/org-init ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (c4/org-theme))

(defun c4/org-theme ()
  (set-face-attribute 'org-block nil
    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-begin-line nil
    :foreground nil :weight 'normal :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-end-line nil
    :foreground nil :weight 'normal :inherit 'fixed-pitch)
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch)))

(provide 'org-setup)
;;;; org.el ends here
