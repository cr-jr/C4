;;; C4 --- CRJr's Coding Command Center
;;;
;;; Commentary:
;;;
;;; This is my personal Emacs configuration for day to day work.
;;; In most cases, it shouldn't be necessary to change this file.
;;; If you want to modify this configuration, the place to start
;;; is C4.org.
;;;
;;; Code:

;;; Set user-emacs-directory & user-init-file properly
(setq user-emacs-directory "~/.config/emacs/")
(setq user-init-file  (concat user-emacs-directory "init.el"))

;;; Load in generated config
(load-file (concat user-emacs-directory "C4.el"))

(provide 'init)
;;; init.el ends here
