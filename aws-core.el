;;; aws-core.el --- Emacs major modes wrapping the AWS CLI

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

(defun aws-core--tabulated-list-from-command (cmd header)
  (let ((rows
         (mapcar
          (lambda (x) `(nil [,x]))
          (split-string
           (shell-command-to-string
            (concat (aws-cmd) cmd)) "\t"))))
    (setq tabulated-list-format header)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))

(defun aws-core--describe-current-resource (cmd)
  "Describe resource under cursor.  CMD is the aws command to describe the resource."
  (let* ((current-resource (tabulated-list-get-id))
         (buffer (concat (aws--buffer-name) ": " cmd " " current-resource "*"))
         (cmd (concat (aws-cmd) "--output yaml " cmd " " current-resource)))
    (call-process-shell-command cmd nil buffer)
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (aws-view-mode))))

(provide 'aws-core)
;;; aws-core.el ends here
