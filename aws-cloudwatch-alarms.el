;;; aws-cloudwatch-alarms.el --- Emacs major modes wrapping the AWS CLI

;; Copyright (C) 2021, Marcel Patzwahl

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Version: 1.0
;; Author: Marcel Patzwahl
;; Keywords: aws cli tools
;; URL: https://github.com/snowiow/aws.el
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Emacs major modes wrapping the AWS CLI

;;; Code:
(require 'transient)

(defun aws-cloudwatch-alarms-describe-alarms ()
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

(defun aws-cloudwatch-alarms-describe-alarms-refresh ()
  "Refresh the CloudWatch Alarms Overview and jump to the last position."
  (interactive)
  (aws-core--refresh-list-view 'aws-cloudwatch-alarms-describe-alarms))

(defun aws-cloudwatch-alarms-describe-alarm ()
  "Describe the alarm under the cursor."
  (interactive)
  (let ((cmd (concat "cloudwatch describe-alarms --alarm-names")))
    (aws-core--describe-current-resource cmd)))

(defun aws-cloudwatch-alarms-enable-disable-alarm ()
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
    (aws-cloudwatch-alarms-describe-alarms)
    (forward-line current-line)
    (message (concat "Executed " action " successfully on " current-alarm))))

(transient-define-prefix aws-cloudwatch-alarms-help-popup ()
  "AWS CloudWatch Alarm Menu"
  ["Actions"
   ("RET" "Describe Alarm"      aws-cloudwatch-alarms-describe-alarm)
   ("P" "Set AWS Profile"       aws-set-profile)
   ("q" "CloudWatch Overview"   aws-cloudwatch)
   ("r" "Refresh Buffer"        aws-cloudwatch-alarms-describe-alarms-refresh)
   ("t" "Toggle ActionsEnabled" aws-cloudwatch-alarms-enable-disable-alarm)])

(defvar aws-cloudwatch-alarms-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-cloudwatch-alarms-describe-alarm)
    (define-key map (kbd "?")   'aws-cloudwatch-alarms-help-popup)
    (define-key map (kbd "P")   'aws-set-profile)
    (define-key map (kbd "q")   'aws-cloudwatch)
    (define-key map (kbd "r")   'aws-cloudwatch-alarms-describe-alarms-refresh)
    (define-key map (kbd "t")   'aws-cloudwatch-alarms-enable-disable-alarm)
    map))

(defun aws-cloudwatch-alarms ()
  "Open AWS CloudWatch Alarms Mode."
  (interactive)
  (aws--pop-to-buffer (aws--buffer-name "cloudwatch-alarms"))
  (aws-cloudwatch-alarms-mode))

(define-derived-mode aws-cloudwatch-alarms-mode tabulated-list-mode "aws-cloudwatch-alarms"
  "AWS CloudWatch Alarms Mode"
  (setq major-mode 'aws-cloudwatch-alarms-mode)
  (use-local-map aws-cloudwatch-alarms-mode-map)
  (aws-cloudwatch-alarms-describe-alarms))

(provide 'aws-cloudwatch-alarms)
;;; aws-cloudwatch-alarms.el ends here
