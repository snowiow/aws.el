;;; aws-s3.el --- Emacs major modes wrapping the AWS CLI

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
;; URL: https://github.com/snowiow/aws.el
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Emacs major modes wrapping the AWS CLI

;;; Code:

(defun aws-s3-lb ()
  (let ((rows (mapcar (lambda (x) `(nil [,x]))
                          (split-string
                           (shell-command-to-string
                            (concat (aws-cmd) "s3 ls | awk '{print $3}'")) "\n"))))
    (fset 'aws--last-view 'aws-s3)
    (setq tabulated-list-format [("Buckets" 100)])
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))

(defvar aws-s3-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws)
    map))

;; (evil-set-initial-state 'aws-s3-mode 'motion)

;; (evil-define-key 'motion aws-s3-mode-map
;;   (kbd "P")   #'aws-set-profile
;;   (kbd "q")   #'aws)

(defun aws-s3 ()
  (interactive)
  (setq aws--current-service "s3")
  (aws--pop-to-buffer (aws--buffer-name))
  (aws-s3-mode))

(define-derived-mode aws-s3-mode tabulated-list-mode "aws-s3"
  "AWS mode"
  (setq major-mode 'aws-s3-mode)
  (use-local-map aws-s3-mode-map)
  (aws-s3-lb))

(provide 'aws-s3)
;;; aws-s3.el ends here
