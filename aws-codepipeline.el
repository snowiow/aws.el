;;; aws-codepipeline.el --- Emacs major modes wrapping the AWS CLI

;; copyright (c) 2022, Marcel Patzwahl

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

(defun aws-codepipeline-list-pipelines ()
  "List all CodePipelines."
  (fset 'aws--last-view 'aws-lambda)
  (aws-core--tabulated-list-from-command
   "codepipeline list-pipelines --output=text --query 'pipelines[*].name'"
   [("CodePipeline" 100)]))

(defun aws-codepipeline--get-pipeline ()
  (interactive)
  (aws-core--describe-current-resource "codepipeline get-pipeline --name"))

(defun aws-codepipeline--get-pipeline-state ()
  (interactive)
  (aws-core--describe-current-resource "codepipeline get-pipeline-state --name"))

(defun aws-codepipeline--list-pipeline-executions ()
  (interactive)
  (aws-core--describe-current-resource "codepipeline list-pipeline-executions --pipeline-name"))

(transient-define-prefix aws-codepipeline-help-popup ()
  "AWS CloudFormation Help Menu"
  ["Actions"
   ("RET" "Get Pipeline" aws)
   ("e" "List Pipeline Executions" aws-codepipeline--list-pipeline-executions)
   ("P" "Set AWS Profile" aws-set-profile)
   ("q" "Service Overview" aws-codepipeline--get-pipeline)
   ("s" "Get Pipeline State" aws-codepipeline--get-pipeline-state)])

(defvar aws-codepipeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-codepipeline--get-pipeline)
    (define-key map (kbd "?") 'aws-codepipeline-help-popup)
    (define-key map (kbd "e") 'aws-codepipeline--list-pipeline-executions)
    (define-key map (kbd "q") 'aws)
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "s") 'aws-codepipeline--get-pipeline-state)
    map))

(defun aws-codepipeline ()
  "Open the AWS Codepipeline Mode."
  (interactive)
  (setq major-mode 'aws-codepipeline-mode)
  (use-local-map aws-codepipeline-mode-map)
  (aws-codepipeline-list-pipelines))

(define-derived-mode aws-codepipeline-mode tabulated-list-mode "aws-codepipeline"
  "AWS CodePipeline Mode"
  (setq major-mode 'aws-codepipeline-mode)
  (use-local-map aws-codepipeline-mode-map))

(provide 'aws-codepipeline)
;;; aws-codepipeline.el ends here
