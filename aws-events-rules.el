;;; aws-events-rules.el --- Emacs major modes wrapping the AWS CLI

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
;; Keywords: aws cli tools
;; URL: https://github.com/snowiow/aws-events-rules.el
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Emacs major modes wrapping the AWS CLI

;;; Code:
(defun aws-events-rules-list-rules ()
  "List all Events Rules."
  (interactive)
  (fset 'aws--last-view 'aws-events-rules)
  (let ((rows
         (mapcar
          (lambda (x)
            (let ((splitted (split-string x "\t")))
              (list (car splitted) (vconcat splitted))))
          (butlast
           (split-string
            (shell-command-to-string
             (concat
              (aws-cmd)
              "events list-rules --output text --query 'Rules[*].[Name,State]'"))"\n")))))
    (setq tabulated-list-format [("Name" 100) ("State" 0)])
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))

(defvar aws-events-rules-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'aws-events)
    map))

(defun aws-events-rules ()
  "Open the Events Rules Mode."
  (interactive)
  (aws--pop-to-buffer (aws--buffer-name "events-rules"))
  (aws-events-rules-mode))

(define-derived-mode aws-events-rules-mode tabulated-list-mode "aws-events-rules"
  "AWS Events Rules mode."
  (setq major-mode 'aws-events-rules-mode)
  (use-local-map aws-events-rules-mode-map)
  (aws-events-rules-list-rules))

(provide 'aws-events-rules)
;;; aws-events-rules.el ends here
