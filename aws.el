;;; aws.el --- Emacs major modes wrapping the AWS CLI

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
;; Keywords: aws cli
;; URL: https://github.com/snowiow/aws.el
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; Emacs major modes wrapping the AWS CLI

;;; Code:
(require 'aws-cloudformation)
(require 'aws-cloudwatch)
(require 'aws-core)
(require 'aws-lambda)
(require 'aws-logs)
(require 'aws-s3)

;;; Code:
(defun aws--list-services ()
  "List available aws services."
  (interactive)
  (let ((rows (list '("cloudformation" ["cloudformation"])
                    '("cloudwatch" ["cloudwatch"])
                    '("lambda" ["lambda"])
                    '("logs" ["logs"])
                    '("s3" ["s3"]))))
    (fset 'aws--last-view 'aws)
    (setq tabulated-list-format [("Services" 100)])
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))

(defun aws--get-service ()
  "Call the respective aws service view, based on the current tabulated-list entry."
  (interactive)
  (let ((service (if (tabulated-list-get-entry)
                     (aref (tabulated-list-get-entry) 0)
                   aws--current-service)))
    (cond ((equal service "cloudformation") (aws-cloudformation))
          ((equal service "cloudwatch") (aws-cloudwatch))
          ((equal service "services") (aws))
          ((equal service "lambda") (aws-lambda))
          ((equal service "logs") (aws-logs))
          ((equal service "s3") (aws-s3))
          (t (message "Hello")))))

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

;; MODE-MAP
(defvar aws-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws--get-service)
    (define-key map (kbd "P")   'aws-set-profile)
    (define-key map (kbd "q")   'aws-quit)
    map))

;; MODE DEFINITION
(defun aws ()
  "Open AWS Major Mode.  This presents a service overview."
  (interactive)
  (setq aws--current-service "services")
  (aws--pop-to-buffer (aws--buffer-name))
  (aws-mode))

(define-derived-mode aws-mode tabulated-list-mode "aws"
  "AWS mode"
  (setq major-mode 'aws-mode)
  (use-local-map aws-mode-map)
  (aws--list-services))

(provide 'aws)
;;; aws.el ends here
