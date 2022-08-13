;;; aws-mode.el --- Emacs major modes wrapping the AWS CLI

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
(require 'aws-cloudformation)
(require 'aws-cloudwatch-alarms)
(require 'aws-cloudwatch)
(require 'aws-codebuild)
(require 'aws-codepipeline)
(require 'aws-core)
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
(require 'aws-view)
(require 'aws-organizations)

(defvar aws-profile (car
                     (split-string
                      (shell-command-to-string "aws configure list-profiles") "\n")))

(defcustom aws-output "yaml"
  "Defines in which format aws outputs are represented."
  :type '(string)
  :group 'aws
  :options '("yaml" "json" "text"))

(defcustom aws-vault nil
  "Set if aws-vault should be used for aws sessions."
  :type 'symbol
  :group 'aws
  :options '('t 'nil))

(fset 'aws--last-view nil)
(setq aws--last-command nil)


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
Use either aws-vault exec or --profile based on setting.
If PROFILE is passed it is used, otherwise the profile set in
aws-profile is used."
  (let ((profile (if profile profile aws-profile)))
    (if aws-vault
        (concat "aws-vault exec "
                profile
                " -- aws ")
      (concat "aws --profile " profile))))

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
  (let ((rows (list '("cloudformation" ["cloudformation"])
                    '("cloudwatch" ["cloudwatch"])
                    '("codebuild" ["codebuild"])
                    '("codepipeline" ["codepipeline"])
                    '("events" ["events"])
                    '("iam" ["iam"])
                    '("lambda" ["lambda"])
                    '("logs" ["logs"])
                    '("s3" ["s3"]))))
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
    (cond ((equal service "cloudformation") (aws-cloudformation))
          ((equal service "cloudwatch") (aws-cloudwatch))
          ((equal service "codebuild") (aws-codebuild))
          ((equal service "codepipeline") (aws-codepipeline))
          ((equal service "events") (aws-events))
          ((equal service "iam") (aws-iam))
          ((equal service "lambda") (aws-lambda))
          ((equal service "logs") (aws-logs))
          ((equal service "s3") (aws-s3))
          (t (message "Hello")))))

(defun aws--get-view-mode ()
  "Return the fitting view mode for aws-output."
  (cond ((equal aws-output "yaml") (aws-view-yaml-mode))
        ((equal aws-output "json") (aws-view-json-mode))
        ((equal aws-output "text") (aws-view-text-mode))
        (t (message "Invalid aws-output '%s' set! Choose one of ['yaml','json','text']" aws-output))))

(defun aws-login ()
  "Login to the AWS Console with a selected profile."
  (interactive)
  (if aws-vault
      (let ((profile (aws-select-profile)))
        (aws--vault-login profile))
    (message "NOT SUPPORTED")))

(defun aws-login-current-account ()
  "Login to the AWS Console with the current profile."
  (interactive)
  (if aws-vault
      (aws--vault-login aws-profile)
    (message "NOT SUPPORTED")))

(defun aws--vault-login (profile)
  "Login to the given PROFILE via aws-vault."
  (shell-command (concat "aws-vault login " profile)))

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
(defun aws ()
  "Open AWS Major Mode.  This presents a service overview."
  (interactive)
  (aws--pop-to-buffer (aws--buffer-name "services"))
  (aws-mode))

(define-derived-mode aws-mode tabulated-list-mode "aws"
  "AWS mode."
  (setq major-mode 'aws-mode)
  (use-local-map aws-mode-map)
  (aws--list-services))

(provide 'aws-mode)
;;; aws-mode.el ends here
