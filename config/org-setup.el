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
    (org-directory path)
    (org-agenda-start-with-log-mode t)
    (org-log-done 'time)
    (org-log-into-drawer t)

    (org-agenda-files
      '("Tasks.org" "Projects.org"))

    (org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
         (sequence
           "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)"
           "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

    (org-refile-targets
      '(("Archive.org" :maxlevel . 1)
         ("Tasks.org" :maxlevel . 1)))

    (org-tag-alist
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

    (org-agenda-custom-commands
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
    (org-capture-templates
      `(("t" "Tasks / Projects")
         ("tt" "Task" entry (file+olp "Tasks.org" "Inbox")
           "* TODO %?\n %U\n %a\n %i" :empty-lines 1)))
    :config
    (advice-add 'org-refile :after 'org-save-all-org-buffers)
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
