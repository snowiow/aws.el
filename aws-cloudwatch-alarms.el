;;; package --- Summary
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;;; Code:
(require 'aws-core)

(defun aws-cloudwatch-describe-alarms ()
  "List all CloudWatch Alarms."
  (interactive)
  (fset 'aws--last-view 'aws-cloudwatch-alarms)
  (let ((rows
         (mapcar
          (lambda (x)
            (let ((splitted (split-string x "\t")))
              (list (car splitted) (vconcat splitted))))
          (butlast
           (split-string
            (shell-command-to-string
             (concat
              (aws-cmd)
              "cloudwatch describe-alarms "
              " --query 'MetricAlarms[*].[AlarmName,StateValue,Namespace,MetricName,Statistic,ActionsEnabled]'"
              " --output text")) "\n")))))
    (setq tabulated-list-format [
                                 ("AlarmName" 85)
                                 ("StateValue" 10)
                                 ("Namespace" 15)
                                 ("MetricName" 20)
                                 ("Statistic" 10)
                                 ("ActionsEnabled" 0)])
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))

(defun aws-cloudwatch-describe-alarm ()
  "Describe the alarm under the cursor."
  (interactive)
  (let ((cmd (concat "cloudwatch describe-alarms --alarm-names")))
    (aws--describe-current-resource cmd)))

(defun aws-cloudwatch-enable-disable-alarm ()
  "Toggle the ActionsEnabled state.
Disable if it's enabled and enable if it's disabled."
  (interactive)
  (message "Updating ActionsEnabled State...")
  (let* ((current-line (+ 1 (count-lines 1 (point))))
         (current-alarm (tabulated-list-get-id))
         (current-state
          (string-trim
           (shell-command-to-string
            (concat
             (aws-cmd)
             "cloudwatch describe-alarms --alarm-names "
             current-alarm
             " --query 'MetricAlarms[0].ActionsEnabled'"))))
         (action (if (string-equal current-state "true")
                     "disable-alarm-actions"
                   "enable-alarm-actions")))
    (shell-command (concat
                    (aws-cmd)
                    "cloudwatch "
                    action
                    " --alarm-names " current-alarm))
    (aws-cloudwatch-describe-alarms)
    (goto-line current-line)
    (message (concat "Executed " action " successfully on " current-alarm))))

(transient-define-prefix aws-cloudwatch-alarms-help-popup ()
  "AWS CloudWatch Alarm Menu"
  ["Actions"
   ("RET" "Describe Alarm"      aws-cloudwatch-describe-alarm)
   ("P" "Set AWS Profile"       aws-set-profile)
   ("q" "CloudWatch Overview"   aws-cloudwatch)
   ("r" "Refresh Buffer"        aws-cloudwatch-describe-alarms)
   ("t" "Toggle ActionsEnabled" aws-cloudwatch-enable-disable-alarm)])

(defvar aws-cloudwatch-alarms-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-cloudwatch-describe-alarm)
    (define-key map (kbd "?")   'aws-cloudwatch-alarms-help-popup)
    (define-key map (kbd "P")   'aws-set-profile)
    (define-key map (kbd "q")   'aws-cloudwatch)
    (define-key map (kbd "r")   'aws-cloudwatch-describe-alarms)
    (define-key map (kbd "t")   'aws-cloudwatch-enable-disable-alarm)
    map))

(defun aws-cloudwatch-alarms ()
  "Open AWS CloudWatch Alarms Mode."
  (interactive)
  (setq aws--current-service "cloudwatch-alarms")
  (aws--pop-to-buffer (aws--buffer-name))
  (aws-cloudwatch-alarms-mode))

(define-derived-mode aws-cloudwatch-alarms-mode tabulated-list-mode "aws-cloudwatch-alarms"
  "AWS CloudWatch Alarms Mode"
  (setq major-mode 'aws-cloudwatch-alarms-mode)
  (use-local-map aws-cloudwatch-alarms-mode-map)
  (aws-cloudwatch-describe-alarms))

(provide 'aws-cloudwatch-alarms)
;;; aws-cloudwatch-alarms.el ends here
