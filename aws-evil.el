;;; aws-evil.el --- Emacs major modes wrapping the AWS CLI

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
(require 'evil)

(defgroup aws-evil nil
  "Provides integration of aws and evil."
  :group 'aws
  :prefix "aws-evil-")

(defvar aws-evil-mode-map (make-sparse-keymap))

;;; Code:
(define-minor-mode aws-evil-mode
  "Brings evil keybindings to aws"
  :lighter "aws-evil"
  :keymap aws-evil-mode-map
  :group 'aws-evil)

;; aws-mode
(evil-define-key 'normal aws-mode-map
  (kbd "RET") #'aws-get-service
  (kbd "?")   #'aws-help-popup
  (kbd "L")   #'aws-login-current-account
  (kbd "P")   #'aws-set-profile
  (kbd "q")   #'aws-quit)

;; aws-cloudformation-mode
(evil-define-key 'normal aws-cloudformation-mode-map
  (kbd "?") #'aws-cloudformation-help-popup
  (kbd "d") #'aws-cloudformation--delete-stack
  (kbd "e") #'aws-cloudformation-describe-stack-events
  (kbd "P") #'aws-set-profile
  (kbd "q") #'aws
  (kbd "r") #'aws-cloudformation-list-stacks-refresh)

;; aws-cloudwatch-mode
(evil-define-key 'normal aws-cloudwatch-mode-map
  (kbd "RET") #'aws-cloudwatch-alarms
  (kbd "P")   #'aws-set-profile
  (kbd "q")   #'aws)

;; aws-cloudwatch-alarms-mode
(evil-define-key 'normal aws-cloudwatch-alarms-mode-map
  (kbd "RET") #'aws-cloudwatch-describe-alarm
  (kbd "?")   #'aws-cloudwatch-alarms-help-popup
  (kbd "P")   #'aws-set-profile
  (kbd "q")   #'aws-cloudwatch
  (kbd "r")   #'aws-cloudwatch-alarms-describe-alarms-refresh
  (kbd "t")   #'aws-cloudwatch-alarms-enable-disable-alarm)

;; aws-events-mode
(evil-define-key 'normal aws-events-mode-map
  (kbd "RET") #'aws-events-rules
  (kbd "q") #'aws)

;; aws-events-rules-mode
(evil-define-key 'normal aws-events-rules-mode-map
  (kbd "q") #'aws-events)

;; aws-lambda-mode
(evil-define-key 'normal aws-lambda-mode-map
  (kbd "RET") #'aws-lambda-get-function
  (kbd "?")   #'aws-lambda-help-popup
  (kbd "e")   #'aws-lambda-event-source-mapping-list-from-line-under-cursor
  (kbd "i")   #'aws-lambda-invoke-popup
  (kbd "L")   #'aws-lambda-get-latest-logs
  (kbd "l")   #'aws-lambda-describe-log-streams
  (kbd "o")   #'aws-lambda-view-last-execution
  (kbd "P")   #'aws-set-profile
  (kbd "q")   #'aws)

;; aws-lambda-event-source-mapping
(evil-define-key 'normal aws-lambda-event-source-mapping-mode-map
  (kbd "RET") #'aws--lambda-get-event-source-mapping
  (kbd "?")   #'aws-lambda-event-source-mapping-help-popup
  (kbd "P")   #'aws-set-profile
  (kbd "q")   #'aws-lambda
  (kbd "r")   #'aws-lambda-event-source-mapping-list-refresh
  (kbd "u")   #'aws-lambda-event-source-mapping-update-popup)

;; aws-logs-mode
(evil-define-key 'normal aws-logs-mode-map
  (kbd "RET")   #'aws-logs-describe-log-group
  (kbd "?")     #'aws-logs-help-popup
  (kbd "P")     #'aws-set-profile
  (kbd "q")     #'aws
  (kbd "s")     #'aws-log-streams-from-line-under-cursor)

;; aws-log-streams
(evil-define-key 'normal aws-log-streams-mode-map
  (kbd "RET") #'aws-log-streams-get-log-event-in-view
  (kbd "P")   #'aws-set-profile
  (kbd "q")   #'aws-logs)

;; aws-s3-mode
(evil-define-key 'normal aws-s3-mode-map
  (kbd "d") #'aws-s3-rb-under-cursor
  (kbd "?") #'aws-s3-help-popup
  (kbd "m") #'aws-s3-mb
  (kbd "P") #'aws-set-profile
  (kbd "q") #'aws
  (kbd "r") #'aws-s3-lb-refresh)

;; aws-view-mode
(evil-define-key 'normal aws-view-mode-map
  (kbd "q") #'kill-current-buffer)

(provide 'aws-evil)
;;; aws-evil.el ends here
