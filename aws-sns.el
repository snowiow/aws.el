;;; aws-sns.el --- Emacs major modes wrapping the AWS CLI

;; Copyright (C) 2025, Marcel Patzwahl

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

;; Author: Marcel Patzwahl

;;; Commentary:

;; Emacs major modes wrapping the AWS CLI

;;; Code:
(require 'transient)

(defun aws-sns--extract-topic-name (arn)
  "Extract the topic name from a topic ARN.
ARN should be in the format: arn:aws:sns:region:account-id:topic-name"
  (car (last (split-string arn ":"))))

(defun aws-sns-list-topics ()
  "List all SNS Topics."
  (interactive)
  (fset 'aws--last-view 'aws-sns)
  (let ((rows
         (mapcar
          (lambda (x)
            (let* ((arn (string-trim x))
                   (topic-name (aws-sns--extract-topic-name arn)))
              (list arn (vector topic-name))))
          (butlast
           (split-string
            (shell-command-to-string
             (concat
              (aws-cmd)
              "sns list-topics "
              " --query 'Topics[*].[TopicArn]'"
              " --output text")) "\n")))))
    (setq tabulated-list-format [
                                 ("Topic Name" 85)])
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))

(defun aws-sns-copy-arn ()
  "Copy the ARN of the topic under the cursor to the kill ring."
  (interactive)
  (let ((arn (tabulated-list-get-id)))
    (kill-new arn)
    (message "Copied ARN to kill ring: %s" arn)))

(transient-define-prefix aws-sns-help-popup ()
  "AWS SNS Help Menu"
  ["Actions"
   ("y" "Copy ARN" aws-sns-copy-arn)
   ("P" "Set AWS Profile" aws-set-profile)
   ("q" "Service Overview" aws)])

(defvar aws-sns-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") 'aws-sns-help-popup)
    (define-key map (kbd "y") 'aws-sns-copy-arn)
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws)
    map))

;;;###autoload
(defun aws-sns ()
  "Open the AWS SNS Mode."
  (interactive)
  (aws--pop-to-buffer (aws--buffer-name "sns"))
  (aws-sns-mode))

(define-derived-mode aws-sns-mode tabulated-list-mode "aws-sns"
  "AWS SNS mode"
  (setq major-mode 'aws-sns-mode)
  (use-local-map aws-sns-mode-map)
  (aws-sns-list-topics))

(provide 'aws-sns)
;;; aws-sns.el ends here

