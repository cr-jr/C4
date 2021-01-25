(defun c4/perf ()
  "Handles performance optimizations"

  ;; Raise the garbage collection threshold high as emacs starts
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))

  ;; Drop it down once loaded
  (add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 1000000))))

(defun c4/housekeeping ()
  "A helper for should-be-defaults"
  ;; Lockfiles do more harm than good
  (setq create-lockfiles nil)

  ;; Custom files just add clutter
  (setq custom-file null-device)

  (add-hook 'before-save-hook
    'delete-trailing-whitespace)    ; Delete trailing whitespace on save

  ;; Create parent dirs when opening new files
  (add-to-list 'find-file-not-found-functions #'c4/create-parent-on-file-find)

  ;; I really don't like clutter. Really :P
  (use-package no-littering
  :custom
  (auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))))

(defun c4/create-parent-on-file-find ()
  "Ensures that the parent dirs are created for a nonexistent file."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
                (y-or-n-p (format
                          "Directory `%s' does not exist! Create it?"
                          parent-directory)))
      (make-directory parent-directory t))))

(defun c4/debugger ()
  "Debugging utilities to ensure optimimal performance."
  (use-package esup)
  (use-package bug-hunter)
  (use-package explain-pause-mode
    :diminish
    :config
    (explain-pause-mode)))

(provide 'preamble)
