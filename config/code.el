;;;; C4 Code
;;;;
;;;; Commentary:
;;;;
;;;; This file contains modules for my software development and
;;;; programming workflow.
;;;;
;;;; Code:

;;; Programming base
(defun c4/code ()
  (use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode)))

(provide 'code)
;;;; code.el ends here
