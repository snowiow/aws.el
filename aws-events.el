;;; aws-events.el --- Emacs major modes wrapping the AWS CLI

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
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Emacs major modes wrapping the AWS CLI

;;; Code:
(defun aws-events--list ()
  "List all Events services."
  (interactive)
  (let ((rows (list '("rules" ["rules"]))))
    (fset 'aws--last-view 'aws-events)
    (setq tabulated-list-format [("Events" 100)])
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))

(defvar aws-events-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'aws)
    (define-key map (kbd "RET") 'aws-events-rules)
    map))

(defun aws-events ()
  "Open the Events Mode."
  (interactive)
  (aws--pop-to-buffer (aws--buffer-name "events"))
  (aws-events-mode))

(define-derived-mode aws-events-mode tabulated-list-mode "aws-events"
  "AWS Events mode."
  (setq major-mode 'aws-events-mode)
  (use-local-map aws-events-mode-map)
  (aws-events--list))

(provide 'aws-events)
;;; aws-events.el ends here
