;;;; C4 Org-mode
;;;;
;;;; Commentary:
;;;;
;;;; This file contains functionality that configures Org-mode and its attendant
;;;; packages for this configuration.
;;;;
;;;; Code:

(cl-defun c4/org (&key path theme)
  (use-package org
    :straight org-plus-contrib
    :hook
    (org-mode . c4/org-init)
    :config
    (eval theme)
    (setq org-ellipsis " ↴"))

  (use-package org-bullets
    :after org
    :hook
    (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list
      '("§" "☙" "❡" "჻" " " " " " "))))

(cl-defun c4/org-theme (&key body headings code base)
  (set-face-attribute 'org-default nil
    :font (format "%s-%s" (or body "Serif") (* base 2)))
  (set-face-attribute 'org-level-1 nil
    :font (format "%s-%s:weight=bold" (or headings "Sans Serif") (* base 2)))
  (set-face-attribute 'org-level-2 nil
    :font (format "%s-%s" (or headings "Sans Serif") (* base 1.75)))
  (set-face-attribute 'org-level-3 nil
    :font (format "%s-%s" (or headings "Sans Serif") (* base 1.5)))
  (set-face-attribute 'org-level-4 nil
    :font (format "%s-%s" (or body "Serif") (* base 1.25)))
  (set-face-attribute 'org-level-5 nil
    :font (format "%s-%s" (or body "Sans Serif") (* base 1)))
  (set-face-attribute 'variable-pitch nil
    :font (format "%s-%s" (or body "Serif") (or base 12)))
  (set-face-attribute 'org-drawer nil
    :font (format "%s-%s" code (or base 12)))
  (set-face-attribute 'org-block nil
    :font (format "%s-%s" code (or base 12)))
  (set-face-attribute 'org-verbatim nil
    :font (format "%s-%s" code (or base 12)))
  (set-face-attribute 'org-code nil
    :font (format "%s-%s" code (or base 12)))
  (set-face-attribute 'org-document-info-keyword nil
    :font (format "%s-%s" code (or base 12)))
  (set-face-attribute 'org-special-keyword nil
    :font (format "%s-%s" code (or base 12)))
  (set-face-attribute 'org-property-value nil
    :font (format "%s-%s" code (or base 12)))
  (set-face-attribute 'org-block-begin-line nil
    :font (format "%s-%s:weight=normal" code (or base 12)))
  (set-face-attribute 'org-block-end-line nil
    :font (format "%s-%s:weight=normal" code (or base 12))))


(defun c4/org-init ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(provide 'org-setup)
;;;; org.el ends here
