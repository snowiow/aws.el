;;; aws-cloudformation.el --- Emacs major modes wrapping the AWS CLI

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

; TODO: When https://github.com/aws/aws-cli/issues/6189 gets intgrated, this list can be inverted. This would end up in just one item
(defvar aws-cloudformation--stack-status-filter "CREATE_IN_PROGRESS CREATE_FAILED CREATE_COMPLETE ROLLBACK_IN_PROGRESS ROLLBACK_FAILED ROLLBACK_COMPLETE DELETE_IN_PROGRESS DELETE_FAILED UPDATE_IN_PROGRESS UPDATE_COMPLETE_CLEANUP_IN_PROGRESS UPDATE_COMPLETE UPDATE_ROLLBACK_IN_PROGRESS UPDATE_ROLLBACK_FAILED UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS UPDATE_ROLLBACK_COMPLETE REVIEW_IN_PROGRESS IMPORT_IN_PROGRESS IMPORT_COMPLETE IMPORT_ROLLBACK_IN_PROGRESS IMPORT_ROLLBACK_FAILED IMPORT_ROLLBACK_COMPLETE")

(defun aws-cloudformation--list-stacks ()
  "List all CloudFormation Stacks.
If POS is set, jump to that line in the view."
  (interactive)
  (fset 'aws--last-view 'aws-cloudformation)
  (let* ((shell-cmd (concat
              (aws-cmd)
              "cloudformation list-stacks "
              "--query 'StackSummaries[*].[StackName,StackStatus]' "
              "--stack-status-filter " aws-cloudformation--stack-status-filter
              " --output text"))
        (rows
         (mapcar
          (lambda (x)
            (let ((splitted (split-string x "\t")))
              (list (car splitted) (vconcat splitted))))
          (butlast
           (split-string
            (shell-command-to-string shell-cmd) "\n")))))
    (setq tabulated-list-format [("Name" 80) ("Status" 0)])
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))

(defun aws-cloudformation-list-stacks-refresh ()
  "Refresh the CloudFormation Stacks Overview and jump to the last position."
  (interactive)
  (aws-core--refresh-list-view 'aws-cloudformation--list-stacks))

(defun aws-cloudformation-describe-stack-events ()
  "Describe the CloudFormation Stack Events of the Stack under the cursor."
  (interactive)
  (aws-core--describe-current-resource "cloudformation describe-stack-events --stack-name"))

(defun aws-cloudformation--delete-stack ()
  "Delete the CloudFormation Stack under the cursor."
  (interactive)
  (let ((stack-name (tabulated-list-get-id)))
    (when (y-or-n-p (concat
                     "Are you sure you want to delete the stack "
                     stack-name
                     "?"))
      (let* ((sub-cmd (concat
                       "cloudformation delete-stack "
                       "--stack-name " stack-name))
             (cmd (concat (aws-cmd) sub-cmd)))
        (shell-command cmd)
        (aws-cloudformation-list-stacks-refresh)
        (message (concat "Triggered deletion on stack " stack-name))))))

(transient-define-prefix aws-cloudformation-help-popup ()
  "AWS CloudFormation Help Menu"
  ["Actions"
   ("d" "Delete Stack" aws-cloudformation--delete-stack)
   ("e" "Describe Stack Events" aws-cloudformation-describe-stack-events)
   ("P" "Set AWS Profile" aws-set-profile)
   ("q" "Service Overview" aws)
   ("r" "Refresh Buffer" aws-cloudformation-list-stacks-refresh)])

(defvar aws-cloudformation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") 'aws-cloudformation-help-popup)
    (define-key map (kbd "d") 'aws-cloudformation--delete-stack)
    (define-key map (kbd "e") 'aws-cloudformation-describe-stack-events)
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws)
    (define-key map (kbd "r") 'aws-cloudformation-list-stacks-refresh)
    map))

(defun aws-cloudformation ()
  "Open the CloudFormation Mode."
  (interactive)
  (aws--pop-to-buffer (aws--buffer-name "cloudformation"))
  (aws-cloudformation-mode))

(define-derived-mode aws-cloudformation-mode tabulated-list-mode "aws-cloudformation"
  "AWS CloudFormation mode"
  (setq major-mode 'aws-cloudformation-mode)
  (use-local-map aws-cloudformation-mode-map)
  (aws-cloudformation--list-stacks))

(provide 'aws-cloudformation)
;;; aws-cloudformation.el ends here
