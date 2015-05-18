(require 'shell-command)
(require 'exec-path-from-shell)

(shell-command-completion-mode)
(exec-path-from-shell-initialize)

;; C-d to kill buffer if process is dead.
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

(defun create-shell ()
    "creates a shell with a given name"
    (interactive);; "Prompt\n shell name:")
    (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))

(defun create-shell-in-new-tab ()
  (interactive)
  (escreen-create-screen)
  (create-shell))

(define-key global-map (kbd "<f1>") 'create-shell-in-new-tab)

(provide 'setup-shell)
