;;; aws-iam-policies.el --- Emacs major modes wrapping the AWS CLI

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

(defun aws-iam-policies--list-policies (&optional attached-to name)
  "List policies.  When ATTACHED-TO and NAME are given only those policies are shown.
ATTACHED-TO can be one of group, role or user.
NAME is the name of the group, role or user."
  (interactive)
  (fset 'aws--last-view 'aws-iam-policies)
  (let* ((cmd (if (and attached-to name)
                  (concat "iam list-attached-"
                          attached-to
                          "-policies --"
                          attached-to
                          "-name "
                          name
                          " --output=text --query 'AttachedPolicies[*].[PolicyArn,PolicyName]'")
                "iam list-policies --output=text --query 'Policies[*].[Arn,PolicyName]'"))
         (policies (split-string (shell-command-to-string (concat (aws-cmd) cmd)) "\n"))
         (rows (mapcar (lambda (x)
                         (let ((splitted (split-string x "\t")))
                           (list (car splitted) (vconcat (last splitted)))))
                       policies)))
    (setq tabulated-list-format [("Managed Policies" 100)])
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))

(defun aws-iam-policies--edit-policy ()
  "Open the policy und the cursor for editing."
  (interactive)
  (let* ((policy-arn (tabulated-list-get-id))
         (policy-version-cmd (concat
                              (aws-cmd)
                              "iam get-policy --policy-arn "
                              policy-arn
                              " --output=text --query 'Policy.DefaultVersionId'"))
         (policy-version (string-trim (shell-command-to-string policy-version-cmd)))
         (subcmd (concat "iam get-policy-version --policy-arn "
                         policy-arn
                         " --version-id "
                         policy-version))
         (cmd (concat (aws-cmd) subcmd " --query 'PolicyVersion.Document'"))
         (buffer (concat "* " subcmd " *")))
    (when (get-buffer buffer)
      (kill-buffer buffer))
    (call-process-shell-command cmd nil buffer)
    (pop-to-buffer buffer)
    (with-current-buffer buffer (json-mode) (aws-iam-policies-edit-mode))
    (goto-char (point-min)))
  (message "Press `C-c C-c` to save the policy or `C-c C-k` to abort"))

(defun aws-iam-policies-create-policy-version ()
  "Apply the policy in the buffer to the one in AWS."
  (interactive)
  (let* ((policy-arn (aws-iam-policies--get-from-buffer-name "--policy-arn"))
         (policy-path (aws-core-write-buffer-to-tmp-file (aws-iam-policies--get-policy-name-from-arn policy-arn)))
         (cmd (concat (aws-cmd)
                      "iam create-policy-version --policy-arn " policy-arn
                      " --policy-document file://" policy-path
                      " --set-as-default")))
    (when (y-or-n-p (concat "Save Policy " policy-arn "?"))
      (let ((output (s-trim (shell-command-to-string cmd))))
        (if (s-starts-with? "An error occurred" output)
            (message output)
          (kill-buffer-and-window)
          (message (concat "Policy " policy-arn " successfully saved!")))))))

(defun aws-iam-policies--get-from-buffer-name (prefix)
  (save-match-data
    ; match anything until the next whitespace
    (string-match (concat prefix "\s*\\([^\s]+\\)") (buffer-name))
    (match-string 1 (buffer-name))))

(defun aws-iam-policies--get-policy-name-from-arn (arn)
  "Return the policy name from the given ARN."
  (save-match-data
    (string-match "arn:aws:iam::[[:digit:]]+:policy/\\([^\s]+\\)" arn)
    (match-string 1 arn)))

;; TRANSIENTS
(transient-define-prefix aws-iam-policies-help-popup ()
  "AWS Lambda Menu"
  ["Actions"
   ("E" "Edit Policy" aws-iam-policies--edit-policy)
   ("P" "Set AWS Profile" aws-set-profile)
   ("q" "Service Overview" aws-iam)])

;; MODE-MAP
(defvar aws-iam-policies-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") 'aws-iam-policies-help-popup)
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "E") 'aws-iam-policies--edit-policy)
    (define-key map (kbd "q") 'aws-iam)
    map))

(defun aws-iam-policies (&optional attached-to name)
  "Open AWS IAM Policies Major Mode.  This presents a list of managed policies."
  (interactive)
  (aws--pop-to-buffer (aws--buffer-name "iam-policies"))
  (aws-iam-policies-mode)
  (aws-iam-policies--list-policies attached-to name))

;; MODE DEFINITION
(define-derived-mode aws-iam-policies-mode tabulated-list-mode "aws-iam-policies"
  "AWS IAM Policies Mode."
  (setq major-mode 'aws-iam-policies-mode)
  (use-local-map aws-iam-policies-mode-map))

(define-minor-mode aws-iam-policies-edit-mode
  "Editing Mode for AWS group Inline Policies."
  :init-value nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'aws-iam-policies-create-policy-version)
            (define-key map (kbd "C-c C-k") 'kill-buffer-and-window)
            map))
  

(provide 'aws-iam-policies)
;;; aws-iam-policies.el ends here
