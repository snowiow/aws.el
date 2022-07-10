;;; aws-logs.el --- Emacs major modes wrapping the AWS CLI

;; Copyright (C) 2022, Marcel Patzwahl

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
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Emacs major modes wrapping the AWS CLI

;;; Code:
(require 'transient)

(defun aws-logs-describe-log-groups ()
  (aws-core--tabulated-list-from-command-multi-column
   "logs describe-log-groups --output=text --query 'logGroups[*].[logGroupName,storedBytes,retentionInDays,metricFilterCount]' --output text"
   [("LogGroupName" 85) ("Stored Bytes" 15) ("Retention" 10) ("Metric Filters" 5)]
   ))

(defun aws-logs-describe-log-group ()
  (interactive)
  (let ((cmd (concat "logs describe-log-groups"
                    " --query 'logGroups[0]'"
                    " --log-group-name-prefix")))
    (aws-core--describe-current-resource cmd)))

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
  (aws--pop-to-buffer (aws--buffer-name "logs"))
  (aws-logs-mode))

(define-derived-mode aws-logs-mode tabulated-list-mode "aws-logs"
  "AWS mode"
  (setq major-mode 'aws-logs-mode)
  (use-local-map aws-logs-mode-map)
  (aws-logs-describe-log-groups))

(provide 'aws-logs)
;;; aws-logs.el ends here
