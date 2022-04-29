;;; aws-cloudwatch.el --- Emacs major modes wrapping the AWS CLI

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

(defun aws-cloudwatch--list ()
  "List all CloudWatch services."
  (interactive)
  (let ((rows (list '("alarms" ["alarms"]))))
    (fset 'aws--last-view 'aws-cloudwatch)
    (setq tabulated-list-format [("CloudWatch" 100)])
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))

;; MODE-MAP
(defvar aws-cloudwatch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-cloudwatch-alarms)
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws)
    map))

;; MODE DEFINITION
(defun aws-cloudwatch ()
  "Open AWS CloudWatch Major Mode.  This presents an overview of CloudWatch subservices."
  (interactive)
  (aws--pop-to-buffer (aws--buffer-name "cloudwatch"))
  (aws-cloudwatch-mode))

(define-derived-mode aws-cloudwatch-mode tabulated-list-mode "aws-cloudwatch"
  "AWS mode"
  (setq major-mode 'aws-cloudwatch-mode)
  (use-local-map aws-cloudwatch-mode-map)
  (aws-cloudwatch--list))

(provide 'aws-cloudwatch)
;;; aws-cloudwatch.el ends here
