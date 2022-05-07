;;; aws-organizations.el --- Emacs major modes wrapping the AWS CLI

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

(defcustom aws-organizations-account nil
  "Set the organizations root account id or a delegated admin account id."
  :type '(string)
  :group 'aws)

(defun aws-organizations-select-account ()
  "Select AWS Account Tuple: [Name Id]."
  (interactive)
  (let ((account (if aws-organizations-account aws-organizations-account aws-profile)))
    (completing-read
     "Select Account: "
     (butlast (split-string
               (shell-command-to-string
                (concat (aws-cmd aws-organizations-account)
                        " organizations list-accounts --query 'Accounts[*].[Name, Id]' --output text"))
               "\n")))))

(defun aws-organizations-get-account-id ()
  "Select AWS Account Tuple: [Name Id] and put ID into clipboard."
  (interactive)
  (let ((account-id (nth 1 (split-string (aws-organizations-select-account)))))
    (kill-new account-id)
    (message "Account ID %S copied to clipboard." account-id)))

(defun aws-organizations-get-account-name ()
  "Select AWS Account Tuple: [Name Id] and put Name into clipboard."
  (interactive)
  (let ((account-id (car (split-string (aws-select-account)))))
    (kill-new account-id)
    (message "Account ID %S copied to clipboard." account-id)))

(provide 'aws-organizations)
;;; aws-organizations.el ends here
