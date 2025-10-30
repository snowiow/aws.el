;;; aws.el --- Emacs major modes wrapping the AWS CLI -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025, Marcel Patzwahl

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
;; Keywords: tools convenience
;; URL: https://github.com/snowiow/aws.el
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "28.1") (transient "0.3.0"))

;;; Commentary:

;; aws.el provides a magit-style interface to the AWS CLI.
;;
;; This package makes heavy use of the transient package and
;; tabulated-list-mode to make the AWS command line interface as
;; discoverable, easy and fast to navigate as possible.
;;
;; Usage:
;;   M-x aws  - Open the AWS service overview
;;   M-x aws-login - Login to AWS Console
;;
;; You can customize the behavior with:
;;   `aws-output' - Output format (yaml, json, or text)
;;   `aws-login-method' - Login method (profile, vault, or sso)

;;; Code:
(require 'transient)
(require 'aws-core)
(require 'aws-view)
(require 'aws-cloudformation)
(require 'aws-cloudwatch-alarms)
(require 'aws-cloudwatch)
(require 'aws-codebuild)
(require 'aws-codepipeline)
(require 'aws-events)
(require 'aws-events-rules)
(require 'aws-iam)
(require 'aws-iam-groups)
(require 'aws-iam-policies)
(require 'aws-lambda-event-source-mapping)
(require 'aws-lambda)
(require 'aws-log-streams)
(require 'aws-logs)
(require 'aws-s3)
(require 'aws-sns)
(require 'aws-organizations)

;; Optional: Bedrock requires markdown-mode
(when (require 'markdown-mode nil t)
  (require 'aws-bedrock nil t))

;;; Customization

(defgroup aws nil
  "Interface to AWS CLI."
  :prefix "aws-"
  :group 'tools
  :link '(url-link :tag "GitHub" "https://github.com/snowiow/aws.el"))

(defcustom aws-output "yaml"
  "Format for AWS command outputs.
Valid options are yaml, json, or text."
  :type '(choice (const :tag "YAML" "yaml")
                 (const :tag "JSON" "json")
                 (const :tag "Text" "text"))
  :group 'aws)

(defcustom aws-login-method 'profile
  "AWS login method to use for authentication.
Options are:
  `profile' - Use aws --profile (default)
  `vault' - Use aws-vault exec
  `sso' - Use aws sso login"
  :type '(choice (const :tag "AWS Profile" profile)
                 (const :tag "AWS Vault" vault)
                 (const :tag "AWS SSO" sso))
  :group 'aws)

(defvar aws-profile nil
  "Currently active AWS profile.")

(defvar aws--last-view nil
  "Function to call to refresh the last view.")

(defvar aws--last-command nil
  "Last AWS command executed.")

;;; Helper Functions

(defun aws--check-cli-installed ()
  "Check if AWS CLI is installed and available.
Signal an error if not found."
  (unless (executable-find "aws")
    (user-error "AWS CLI not found.  Please install it from https://aws.amazon.com/cli/")))

(defun aws--initialize-profile ()
  "Initialize aws-profile if not already set."
  (unless aws-profile
    (aws--check-cli-installed)
    (setq aws-profile (car
                       (split-string
                        (shell-command-to-string "aws configure list-profiles") "\n")))))

(fset 'aws--last-view nil)


(defun aws--buffer-name (service)
  "Return aws.el buffer name.
SERVICE represents the currently active service"
  (concat (format "*aws.el [profile: %s] [service: %s]" aws-profile service) "*"))

(defun aws--pop-to-buffer (name)
  "Create a buffer with NAME if not exists and switch to it."
  (unless (get-buffer name)
    (get-buffer-create name))
    (pop-to-buffer-same-window name))

(defun aws-cmd (&optional profile)
  "Create the AWS base cmd with the right profile.
Use aws-vault exec, aws-sso exec, or --profile based on aws-login-method setting.
If PROFILE is passed it is used, otherwise the profile set in
aws-profile is used."
  (aws--initialize-profile)
  (let ((profile (if profile profile aws-profile)))
    (cond ((eq aws-login-method 'vault)
           (concat "aws-vault exec "
                   profile
                   " -- aws "))
          ((eq aws-login-method 'sso)
           (concat "aws-sso exec -p "
                   profile
                   " -- aws "))
          (t
           (concat "aws --profile " profile " ")))))

(defun aws-select-profile ()
  "Select the active AWS Profile."
  (interactive)
  (completing-read
   "Select profile: "
   (split-string (shell-command-to-string "aws configure list-profiles") "\n")))

(defun aws-set-profile ()
  "Set active AWS Profile."
  (interactive)
  (setq aws-profile (aws-select-profile))
  (aws--last-view))

(defun aws-quit ()
  "Quits the aws major mode by killing all it's open buffers."
  (interactive)
  (when (y-or-n-p "Are you sure you want to quit? ")
    (let* ((buffer-names (mapcar (lambda (x) (buffer-name x)) (buffer-list)))
           (aws-buffers (seq-filter
                         (lambda (x) (string-match-p "\\*aws\\.el\s.+\\*" x))
                         buffer-names)))
      (dolist (buffer-name aws-buffers)
        (kill-buffer buffer-name)))))

(defun aws--list-services ()
  "List available aws services."
  (interactive)
  (let ((rows (append
               (when (featurep 'aws-bedrock)
                 '(("bedrock" ["bedrock"])))
               '(("cloudformation" ["cloudformation"])
                 ("cloudwatch" ["cloudwatch"])
                 ("codebuild" ["codebuild"])
                 ("codepipeline" ["codepipeline"])
                 ("events" ["events"])
                 ("iam" ["iam"])
                 ("lambda" ["lambda"])
                 ("logs" ["logs"])
                 ("s3" ["s3"])
                 ("sns" ["sns"])))))
    (fset 'aws--last-view 'aws)
    (setq tabulated-list-format [("Services" 100)])
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))

(defun aws-get-service ()
  "Call the respective aws service view, based on the current tabulated-list entry."
  (interactive)
  (let ((service (tabulated-list-get-id)))
    (cond ((equal service "bedrock") (aws-bedrock))
          ((equal service "cloudformation") (aws-cloudformation))
          ((equal service "cloudwatch") (aws-cloudwatch))
          ((equal service "codebuild") (aws-codebuild))
          ((equal service "codepipeline") (aws-codepipeline))
          ((equal service "events") (aws-events))
          ((equal service "iam") (aws-iam))
          ((equal service "lambda") (aws-lambda))
          ((equal service "logs") (aws-logs))
          ((equal service "s3") (aws-s3))
          ((equal service "sns") (aws-sns))
          (t (message "Hello")))))

(defun aws--get-view-mode ()
  "Return the fitting view mode for aws-output."
  (cond ((equal aws-output "yaml") (aws-view-yaml-mode))
        ((equal aws-output "json") (aws-view-json-mode))
        ((equal aws-output "text") (aws-view-text-mode))
        (t (message "Invalid aws-output '%s' set! Choose one of ['yaml','json','text']" aws-output))))

;;;###autoload
(defun aws-login ()
  "Login to the AWS Console with a selected profile."
  (interactive)
  (aws--check-cli-installed)
  (let ((profile (aws-select-profile)))
    (cond ((eq aws-login-method 'vault)
           (aws--vault-login profile))
          ((eq aws-login-method 'sso)
           (aws--sso-login profile))
          (t
           (message "Console login only supported with 'vault or 'sso login methods")))))

(defun aws-login-current-account ()
  "Login to the AWS Console with the current profile."
  (interactive)
  (cond ((eq aws-login-method 'vault)
         (aws--vault-login aws-profile))
        ((eq aws-login-method 'sso)
         (aws--sso-login aws-profile))
        (t
         (message "Console login only supported with 'vault or 'sso login methods"))))

(defun aws--vault-login (profile)
  "Login to the given PROFILE via aws-vault."
  (shell-command (concat "aws-vault login " profile)))

(defun aws--sso-login (profile)
  "Login to the given PROFILE via aws-sso."
  (shell-command (concat "aws-sso login -p " profile)))

(transient-define-prefix aws-help-popup ()
  "AWS CloudFormation Help Menu"
  ["Actions"
   ("RET" "Get selected Service" aws-get-service)
   ("L" "Login to current Account" aws-login-current-account)
   ("P" "Set AWS Profile" aws-set-profile)
   ("q" "Quit AWS Mode" aws-quit)])

;; MODE-MAP
(defvar aws-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-get-service)
    (define-key map (kbd "?")   'aws-help-popup)
    (define-key map (kbd "L")   'aws-login-current-account)
    (define-key map (kbd "P")   'aws-set-profile)
    (define-key map (kbd "q")   'aws-quit)
    map))

;; MODE DEFINITION
;;;###autoload
(defun aws ()
  "Open AWS Major Mode.  This presents a service overview."
  (interactive)
  (aws--check-cli-installed)
  (aws--initialize-profile)
  (aws--pop-to-buffer (aws--buffer-name "services"))
  (aws-mode))

(define-derived-mode aws-mode tabulated-list-mode "aws"
  "AWS mode."
  (setq major-mode 'aws-mode)
  (use-local-map aws-mode-map)
  (aws--list-services))

(provide 'aws-mode)
(provide 'aws)
;;; aws.el ends here
