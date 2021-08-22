;;; package --- Summary
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;;; Code:
(require 'aws-core)
(require 'aws-utils)
(require 'aws-log-streams)

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
                          "logs describe-log-groups --output=text --query 'logGroups[*].[logGroupName,storedBytes,retentionInDays,metricFilterCount]' --output text")) "\n")))))
    ;; (message column-width)))
    (setq tabulated-list-format [("LogGroupName" 85) ("Stored Bytes" 15) ("Retention" 10) ("Metric Filters" 5) ])
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))

(defun aws-logs-describe-log-group ()
  (interactive)
  (let ((cmd (concat "logs describe-log-groups"
                    " --query 'logGroups[0]'"
                    " --log-group-name-prefix")))
    (aws--describe-current-resource cmd)))

(transient-define-prefix aws-logs-help-popup ()
  "AWS Logs Menu"
  ["Actions"
   ("RET" "Describe Log Group" aws-logs-describe-log-group)
   ("P" "Set AWS Profile" aws-set-profile)
   ("s" "Get Log Streams" aws-log-streams-from-line-under-cursor)
   ("q" "Service Overview" aws)])

(defvar aws-logs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-logs-describe-log-group)
    (define-key map (kbd "?")   'aws-logs-help-popup)
    (define-key map (kbd "P")   'aws-set-profile)
    (define-key map (kbd "q")   'aws)
    (define-key map (kbd "s")   'aws-log-streams-from-line-under-cursor)
    map))

(defun aws-logs ()
  (interactive)
  (setq aws--current-service "logs")
  (aws--pop-to-buffer (aws--buffer-name))
  (aws-logs-mode))

(define-derived-mode aws-logs-mode tabulated-list-mode "aws-logs"
  "AWS mode"
  (setq major-mode 'aws-logs-mode)
  (use-local-map aws-logs-mode-map)
  (aws--logs-describe-log-groups))

(provide 'aws-logs)
;;; aws-logs.el ends here
