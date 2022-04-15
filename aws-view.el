;;; aws-view.el --- Emacs major modes wrapping the AWS CLI

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
(defun aws-view--open-under-cursor ()
  (interactive)
  (let ((current-line-content
         (string-trim
          (buffer-substring-no-properties
           (line-beginning-position) (line-end-position)))))
    (when-let ((codebuild-project
                (aws-view--yaml-get-codebuild-project current-line-content)))
      (aws-codebuild--get-project codebuild-project))))

(defun aws-view--yaml-get-codebuild-project (content)
  (save-match-data
    (string-match "ProjectName:\s*\\(.+\\)" content)
    (match-string 1 content)))

(defvar aws-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'kill-current-buffer)
    (define-key map (kbd "RET") 'aws-view--open-under-cursor)
    map))

(define-derived-mode aws-view-yaml-mode yaml-mode "aws-view-yaml"
  "AWS mode"
  (setq major-mode 'aws-view-yaml-mode)
  (use-local-map aws-view-mode-map)
  (view-mode))

(define-derived-mode aws-view-json-mode json-mode "aws-view-json"
  "AWS mode"
  (setq major-mode 'aws-view-json-mode)
  (use-local-map aws-view-mode-map)
  (view-mode))

(define-derived-mode aws-view-text-mode fundamental-mode "aws-view-text"
  "AWS mode"
  (setq major-mode 'aws-view-text-mode)
  (use-local-map aws-view-mode-map)
  (view-mode))

(provide 'aws-view)
;;; aws-view.el ends here
