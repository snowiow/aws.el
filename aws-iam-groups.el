;;; aws-iam-groups.el --- Emacs major modes wrapping the AWS CLI

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
(require 'aws-iam-group-inline-policies)

(defun aws-iam-groups--list-groups ()
  "List all Lambda Functions."
  (fset 'aws--last-view 'aws-iam-groups)
  (aws-core--tabulated-list-from-command
   "iam list-groups --output=text --query 'Groups[*].GroupName'"
   [("IAM Groups" 100)]))

(defun aws-iam-groups--list-group-inline-policies ()
  "List Inline Policies for group under cursor."
  (interactive)
  (let ((group (tabulated-list-get-id)))
    (aws-iam-group-inline-policies group)))

(defun aws-iam-groups--list-attached-group-policies ()
  "List attached policies for group under cursor."
  (interactive)
  (let ((group-name (tabulated-list-get-id)))
    (aws-iam-policies "group" group-name)))

(defun aws-iam-groups--get-group ()
  "Describe current group."
  (interactive)
  (aws-core--describe-current-resource "iam get-group --group-name "))

;; TRANSIENTS
(transient-define-prefix aws-iam-groups-help-popup ()
  "AWS Lambda Menu"
  ["Actions"
   ("RET" "Get Group" aws-iam-groups--get-group)
   ("i" "List Group Inline Policies" aws-iam-groups--list-group-inline-policies)
   ("a" "List Attached Group Policies" aws-iam-groups--list-attached-group-policies)
   ("P" "Set AWS Profile" aws-set-profile)
   ("q" "Service Overview" aws-iam)])

;; MODE-MAP
(defvar aws-iam-groups-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") 'aws-iam-groups-help-popup)
    (define-key map (kbd "RET") 'aws-iam-groups--get-group)
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "a") 'aws-iam-groups--list-attached-group-policies)
    (define-key map (kbd "i") 'aws-iam-groups--list-group-inline-policies)
    (define-key map (kbd "q") 'aws-iam)
    map))

;; MODE DEFINITION
(defun aws-iam-groups ()
  "Open AWS IAM Groups Major Mode.  This presents an overview of IAM subservices."
  (interactive)
  (aws--pop-to-buffer (aws--buffer-name "iam-groups"))
  (aws-iam-groups-mode))

(define-derived-mode aws-iam-groups-mode tabulated-list-mode "aws-iam-groups"
  "AWS IAM Groups Mode."
  (setq major-mode 'aws-iam-groups-mode)
  (use-local-map aws-iam-groups-mode-map)
  (aws-iam-groups--list-groups))

(provide 'aws-iam-groups)
;;; aws-iam-groups.el ends here
