(require 'aws-core)
(require 'aws-log-streams)

(defun aws-logs-describe-log-groups ()
  (fset 'aws-last-view 'aws-logs)
  (aws--tabulated-list-from-command
   "logs describe-log-groups --output=text --query 'logGroups[*].logGroupName'"
   [("Log Groups" 100)]
   ))

(defvar aws-logs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-log-streams-from-line-under-cursor)
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws)
    map))

(defun aws-logs ()
  (interactive)
  (setq aws-current-service "logs")
  (aws--pop-to-buffer (aws--buffer-name))
  (aws-logs-mode))

(define-derived-mode aws-logs-mode tabulated-list-mode "aws logs"
  "AWS mode"
  (setq major-mode 'aws-logs-mode)
  (use-local-map aws-logs-mode-map)
  (aws-logs-describe-log-groups))

(provide 'aws-logs)
