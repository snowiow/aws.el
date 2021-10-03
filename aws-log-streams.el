;;; aws-log-streams.el --- Emacs major modes wrapping the AWS CLI

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

(defvar-local aws-log-streams-current-group-name nil)

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
  (aws-core--tabulated-list-from-command
   (aws-log-streams-get-latest-logs-command log-group-name)
   [("Log Streams" 100)]))

(defun aws-log-streams-get-log-event-in-view ()
  (interactive)
  (let ((current-log-stream-name (aref (tabulated-list-get-entry) 0)))
    (aws-log-streams-get-log-event aws-log-streams-current-group-name current-log-stream-name)))

(defun aws-log-streams-get-log-event (log-group log-stream)
  (let ((buffer (concat "*" log-group ": " log-stream "*"))
        (cmd (concat
              (aws-cmd)
              "logs get-log-events --log-group-name '" log-group
              "' --log-stream-name '" log-stream "'"
               " --output=" aws-output)))
    (call-process-shell-command cmd nil buffer)
    (switch-to-buffer buffer)
    (with-current-buffer buffer (aws--get-view-mode))))

(defvar aws-log-streams-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-log-streams-get-log-event-in-view)
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
  (aws--pop-to-buffer (aws--buffer-name "log-streams"))
  (aws-log-streams-mode)
  (setq-local aws-log-streams-current-group-name log-group-name)
  (aws-log-streams-describe-log-streams log-group-name))

(define-derived-mode aws-log-streams-mode tabulated-list-mode "aws-log-streams"
  "AWS Log Stream Mode"
  (setq major-mode 'aws-log-streams-mode)
  (use-local-map aws-log-streams-mode-map))

(provide 'aws-log-streams)
;;; aws-log-streams.el ends here
