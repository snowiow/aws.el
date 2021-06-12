(require 'aws-core)

(defvar-local current-log-group-name nil)

(defun aws-log-streams-get-latest-logs-command (log-group-name &optional count)
  (let ((max-items-string (if count
                             (concat "--max-items " count)
                           "")))
    (concat "logs describe-log-streams --log-group-name '"
            log-group-name
            "' --output=text --query 'logStreams[*].logStreamName'"
            " --order-by LastEventTime "
            " --descending "
            max-items-string)))

(defun aws-log-streams-describe-log-streams (log-group-name)
  (aws--tabulated-list-from-command
   (aws-log-streams-get-latest-logs-command log-group-name)
   [("Log Streams" 100)]))

(defun aws-log-get-log-events-in-log-streams-view ()
  (interactive)
  (let ((current-log-stream-name (aref (tabulated-list-get-entry) 0)))
    (aws-log-get-log-events current-log-group-name current-log-stream-name)))

(defun aws-log-get-log-events (log-group log-stream)
  (let ((buffer (concat "*" log-group ": " log-stream "*"))
        (cmd (concat
              (aws-cmd)
              "logs get-log-events --log-group-name '" log-group
              "' --log-stream-name '" log-stream "'")))
    (call-process-shell-command cmd nil buffer)
    (switch-to-buffer buffer)
    (with-current-buffer buffer (aws-view-mode))))

(defvar aws-log-streams-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-log-get-log-events-in-log-streams-view)
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws-logs)
    map))

(defun aws-log-streams-from-line-under-cursor ()
  "Get the Log Streams for the Log Group under the cursor.
Used from the aws-logs mode."
  (interactive)
  (let ((log-group-name (car
                         (split-string
                          (thing-at-point 'line)))))
    (aws-log-streams log-group-name)))

(defun aws-log-streams (log-group-name)
  (interactive "slog-group name: ")
  (setq aws--current-service "log-streams")
  (aws--pop-to-buffer (aws--buffer-name))
  (aws-log-streams-mode)
  (setq-local current-log-group-name log-group-name)
  (aws-log-streams-describe-log-streams log-group-name))

(define-derived-mode aws-log-streams-mode tabulated-list-mode "aws-log-streams"
  "AWS Log Stream Mode"
  (setq major-mode 'aws-log-streams-mode)
  (use-local-map aws-log-streams-mode-map))

(provide 'aws-log-streams)
;;; aws-log-streams.el ends here
