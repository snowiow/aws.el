;;; aws-core.el --- Emacs major modes wrapping the AWS CLI

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

(defun aws-core--tabulated-list-from-command (cmd header)
  "Displays an aws service list command in a tabulated-list-view.
CMD is the aws command to get the resources to list.
HEADER configures the column header for the tabulated-list-view."
  (let ((rows
         (mapcar
          (lambda (x)
            (let ((trimmed-x (string-trim x)))
                (list trimmed-x (vector trimmed-x))))
          (split-string
           (shell-command-to-string
            (concat (aws-cmd) cmd)) "\t"))))
    (setq tabulated-list-format header)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))

(defun aws-core--describe-current-resource (cmd &optional name)
  "Describe resource under cursor.  CMD is the aws command to describe the resource."
  (let* ((current-resource (if name
                               name
                             (tabulated-list-get-id)))
         (service-name (car (split-string cmd)))
         (buffer (concat (aws--buffer-name service-name) ": " cmd " " current-resource "*"))
         (cmd (concat (aws-cmd)
                      "--output "
                      aws-output
                      " "
                      cmd
                      " "
                      current-resource)))
    (call-process-shell-command cmd nil buffer)
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (aws--get-view-mode))
    (goto-line 1)))

(defun aws-core--get-current-line ()
  "Get the current line where point is."
  (+ 1 (count-lines 1 (point))))

(defun aws-core--refresh-list-view (list-function &rest args)
  "Refresh the current tabulated list view.
LIST-FUNCTION is the function to load the list and ARGS will
be passed to LIST-FUNCTION if needed."
  (message "Refreshing buffer...")
  (if (> (length args) 0)
      (funcall list-function args)
    (funcall list-function))
  (forward-line (aws-core--get-current-line))
  (message "Buffer refreshed"))

(provide 'aws-core)
;;; aws-core.el ends here
