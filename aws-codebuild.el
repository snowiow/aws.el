;;; aws-codebuild.el --- Emacs major modes wrapping the AWS CLI

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
(defun aws-codebuild--list-projects ()
  "List all Codebuild Projects."
  (fset 'aws--last-view 'aws-codebuild)
  (aws-core--tabulated-list-from-command
   "codebuild list-projects --output=text --query projects"
   [("Projects" 100)]))

(defun aws-codebuild--get-project (codebuild-project-name)
  "Get Codebuild Project under cursor."
  (interactive)
  (aws-core--describe-current-resource "codebuild batch-get-projects --query 'projects[0]' --names " codebuild-project-name))

;; TRANSIENTS
(transient-define-prefix aws-codebuild-help-popup ()
  "AWS Codebuild Menu"
  ["Actions"
   ("RET" "Get Codebuild Project" aws-codebuild--get-project)
   ("P" "Set AWS Profile" aws-set-profile)
   ("q" "Service Overview" aws)])

(defvar aws-codebuild-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") 'aws-codebuild-help-popup)
    (define-key map (kbd "RET") 'aws-codebuild--get-project)
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws)
    map))

(defun aws-codebuild ()
  "Open the AWS Codebuild Mode."
  (interactive)
  (aws--pop-to-buffer (aws--buffer-name "codebuild"))
  (aws-codebuild-mode))

(define-derived-mode aws-codebuild-mode tabulated-list-mode "aws-codebuild"
  "AWS Codebuild mode"
  (setq major-mode 'aws-codebuild-mode)
  (use-local-map aws-codebuild-mode-map)
  (aws-codebuild--list-projects))

(provide 'aws-codebuild)
;;; aws-codebuild.el ends here
