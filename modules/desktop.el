(defun c4/desktop ()
  "A module for my EXWM configuration."
  (use-package exwm
    :if window-system
    :diminish
    :custom
    (c4/desktop-init)
    (c4/desktop-key-def)
    :config
    ;; Startup process
    (start-process-shell-command
      "nitrogen" nil "nitrogen --restore")

    (display-time-mode t)
    (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

    ;; Update window class with the buffer name
    (add-hook 'exwm-update-class-hook #'c4/exwm-update-class)

    (c4/desktop-randr)


    (exwm-enable))

  ;; EXWM: Desktop Environment
  (use-package desktop-environment
    :after exwm
    :diminish
    :bind
    ("s-l" . windmove-right)
    :config
    (desktop-environment-mode)))

(defun c4/desktop-init ()
  (exwm-workspace-number 6)
  (exwm-randr-workspace-monitor-plist
  '(0 "LVDS" 2 "LVDS" 4 "LVDS" 1 "HDMI-0" 3 "HDMI-0" 5 "HDMI-0"))
  (exwm-systemtray-height 16)
  (display-time-default-load-average nil)
  (exwm-workspace-warp-cursor t)
  (focus-follows-mouse t))

(defun c4/desktop-key-def ()
  (exwm-input-prefix-keys
      '(?\C-x
      ?\C-u
      ?\C-h
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:
      ?\s-\ ))
  (exwm-input-global-keys
    `(([?\s-K] . windmove-swap-states-up)
    ([?\s-k] . windmove-up)
    ([?\s-L] . windmove-swap-states-right)
    ([?\s-l] . windmove-right)
    ([?\s-J] . windmove-swap-states-down)
    ([?\s-j] . windmove-down)
    ([?\s-H] . windmove-swap-states-left)
    ([?\s-h] . windmove-left)
    ([?\s-r] . exwm-reset)
    ([?\s-Q] . exwm-exit)
    ([?\s-q] . exwm-restart)
    ([?\s-W] . exwm-workspace-swap)
    ([?\s-w] . exwm-workspace-switch)
    ([?\s-D] . counsel-linux-app)
    ([?\s-d] . (lambda (cmd)
    (interactive (list (read-shell-command "$ ")))
    (start-process-shell-command cmd nil cmd)))
    ,@(mapcar (lambda (i)
    `(,(kbd (format "s-%d" i)) .
        (lambda ()
        (interactive)
        (exwm-workspace-switch-create ,i))))
    (number-sequence 0 9)))))

(defun c4/desktop-randr ()
  (require 'exwm-randr)
  (exwm-randr-enable)
  (start-process-shell-command "xrandr" nil
    (concat user-emacs-directory "desktop/multihead.sh")))

(defun c4/desktop-systray ()
  (require 'exwm-systemtray)
  (exwm-systemtray-enable))

(provide 'desktop)
