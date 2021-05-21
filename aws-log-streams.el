(require 'aws-core)

(defun aws-log-describe-log-streams (log-group-name)
  (fset 'aws-last-view 'aws-logs)
  (aws--tabulated-list-from-command
   (concat "logs describe-log-streams --log-group-name '"
           log-group-name
           "' --output=text --query 'logStreams[*].logStreamName'")
   [("Log Streams" 100)]
   ))

(defvar aws-log-streams-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws-logs)
    map))

(defun aws-log-streams-from-line-under-cursor ()
  (interactive)
  (let ((log-group-name (string-trim (thing-at-point 'line))))
    (aws-log-streams log-group-name)))

(defun aws-log-streams (log-group-name)
  (interactive "slog-stream name: ")
  (setq aws-current-service "log-streams")
  (aws--pop-to-buffer (aws--buffer-name))
  (aws-log-streams-mode)
  (aws-log-describe-log-streams log-group-name))

(define-derived-mode aws-log-streams-mode tabulated-list-mode "aws log-streams"
  "AWS mode"
  (setq major-mode 'aws-log-streams-mode)
  (use-local-map aws-log-streams-mode-map))

(provide 'aws-log-streams)
