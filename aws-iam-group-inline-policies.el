;;; aws-iam-group-inline-policies.el --- Emacs major modes wrapping the AWS CLI

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
(require 'aws-core)

(defun aws-iam-group-inline-policies--list (group)
  "List all IAM Group Inline Policies for GROUP."
  (fset 'aws--last-view 'aws-iam-group-inline-policies)
  (aws-core--tabulated-list-from-command
   (concat "iam list-group-policies --group-name "
           group
           "  --output=text --query 'PolicyNames'")
   [("IAM Group Inline Policies" 100)]))

(defun aws-iam-group-inline-policies--edit-policy ()
  "Open the policy und the cursor for editing."
  (interactive)
  (let* ((policy-name (tabulated-list-get-id))
         (group-name (aws-iam-group-inline-policies--get-from-buffer-name "group:"))
         (subcmd (concat "iam get-group-policy --group-name " group-name " --policy-name " policy-name))
         (cmd (concat (aws-cmd) subcmd " --query 'PolicyDocument'"))
         (buffer (concat "* " subcmd " *")))
    (when (get-buffer buffer)
      (kill-buffer buffer))
    (call-process-shell-command cmd nil buffer)
    (pop-to-buffer buffer)
    (with-current-buffer buffer (json-mode) (aws-iam-group-inline-policies-edit-mode))
    (goto-char (point-min)))
  (message "Press `C-c C-c` to save the policy or `C-c C-k` to abort"))

(defun aws-iam-group-inline-policies-put-group-policy ()
  "Apply the policy in the buffer to the one in AWS."
  (interactive)
  (let* ((group-name (aws-iam-group-inline-policies--get-from-buffer-name "--group-name"))
         (policy-name (aws-iam-group-inline-policies--get-from-buffer-name "--policy-name"))
         (policy-path (aws-core-write-buffer-to-tmp-file (concat group-name policy-name)))
         (cmd (concat (aws-cmd)
                      "iam put-group-policy --group-name " group-name
                      " --policy-name " policy-name
                      " --policy-document file://" policy-path)))
    (when (y-or-n-p (concat "Save Inline Policy " policy-name " for group " group-name "?"))
      (let ((output (s-trim (shell-command-to-string cmd))))
        (if (not (string-empty-p output))
            (message output)
          (kill-buffer-and-window)
          (message (concat "Policy " policy-name " for group " group-name " successfully saved!")))))))


(defun aws-iam-group-inline-policies--get-from-buffer-name (prefix)
  (save-match-data
    ; match anything until the next whitespace
    (string-match (concat prefix "\s*\\([^\s]+\\)") (buffer-name))
    (match-string 1 (buffer-name))))

;; TRANSIENTS
(transient-define-prefix aws-iam-group-inline-policies-help-popup ()
  "AWS Lambda Menu"
  ["Actions"
   ("E" "Edit Inline Policy" aws-iam-group-inline-policies--edit-policy)
   ("P" "Set AWS Profile" aws-set-profile)
   ("q" "Service Overview" aws-iam-groups)])

;; MODE-MAP
(defvar aws-iam-group-inline-policies-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") 'aws-iam-group-inline-policies-help-popup)
    (define-key map (kbd "E") 'aws-iam-group-inline-policies--edit-policy)
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws-iam-groups)
    map))

;; MODE DEFINITIONS
(defun aws-iam-group-inline-policies (group)
  "Open AWS IAM Group Policies Major Mode."
  (interactive)
  (aws--pop-to-buffer (aws--buffer-name (concat "iam-group-inline-policies, group: " group " ")))
  (aws-iam-group-inline-policies-mode)
  (aws-iam-group-inline-policies--list group))

(define-derived-mode aws-iam-group-inline-policies-mode tabulated-list-mode "aws-iam-group-inline-policies"
  "AWS IAM Group Policies mode."
  (setq major-mode 'aws-iam-group-inline-policies)
  (use-local-map aws-iam-group-inline-policies-mode-map))

(define-minor-mode aws-iam-group-inline-policies-edit-mode
  "Editing Mode for AWS group Inline Policies."
  :init-value nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'aws-iam-group-inline-policies-put-group-policy)
            (define-key map (kbd "C-c C-k") 'kill-buffer-and-window)
            map))

(provide 'aws-iam-group-inline-policies)
;;; aws-iam-group-inline-policies.el ends here
