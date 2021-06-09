(require 'aws-core)
(require 'aws-utils)
(require 'aws-log-streams)

;;; Code:
(defun aws--logs-describe-log-groups ()
  "List all log groups by it's names."
  (fset 'aws--last-view 'aws-logs)
  (let* ((rows (mapcar (lambda (x)
                      (let ((splitted (split-string x "\t")))
                        (list (car splitted) (vconcat splitted))))
                      (butlast
                       (split-string
                        (shell-command-to-string
                         (concat
                          (aws-cmd)
                          "logs describe-log-groups --output=text --query 'logGroups[*].[logGroupName,storedBytes,retentionInDays,metricFilterCount]' --output text")) "\n"))))
         (column-width (longest-element (mapcar 'car rows))))
    ;; (message column-width)))
    (setq tabulated-list-format [("LogGroupName" 85) ("Stored Bytes" 15) ("Retention" 10) ("Metric Filters" 5) ])
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))
  ;; (aws--tabulated-list-from-command
  ;;  "logs describe-log-groups --output=text --query 'logGroups[*].logGroupName'"
  ;;  [("Log Groups" 100)]
  ;;  ))

(defun aws-logs-describe-log-group ()
  (interactive)
  (let* ((current-log-group (aref (tabulated-list-get-entry) 0))
         (buffer (concat "*aws.el [log-group info]:" current-log-group "*"))
         (cmd (concat
               (aws-cmd)
               "logs describe-log-groups"
               " --log-group-name-prefix " current-log-group
               " --query 'logGroups[0]'"
               " --output yaml")))
    (call-process-shell-command cmd nil buffer)
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (aws-view-mode))))

(defvar aws-logs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-log-streams-from-line-under-cursor)
    (define-key map (kbd "i")   'aws-logs-describe-log-group)
    (define-key map (kbd "P")   'aws-set-profile)
    (define-key map (kbd "q")   'aws)
    map))

(defun aws-logs ()
  (interactive)
  (setq aws--current-service "logs")
  (aws--pop-to-buffer (aws--buffer-name))
  (aws-logs-mode))

(define-derived-mode aws-logs-mode tabulated-list-mode "aws logs"
  "AWS mode"
  (setq major-mode 'aws-logs-mode)
  (use-local-map aws-logs-mode-map)
  (aws--logs-describe-log-groups))

(provide 'aws-logs)
;;; aws-logs.el ends here
