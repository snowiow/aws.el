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
  "List all S3 Buckets in a tabulated list view."
  (interactive)
  (fset 'aws--last-view 'aws-s3)
  (let ((rows (mapcar (lambda (x)
                        (let ((splitted (split-string x "\s")))
                          (list (car (last splitted)) (vector (last splitted)))))
                          (split-string
                           (shell-command-to-string
                            (concat (aws-cmd) "s3 ls")) "\n"))))
    (setq tabulated-list-format [("Buckets" 100)])
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))

(defun aws-s3-lb-refresh ()
  "Refresh the S3 Overview and jump to the last position."
  (interactive)
  (aws-core--refresh-list-view 'aws-s3-lb))

(defun aws-s3-mb (bucket-name)
  "Create an S3 Bucket with the name BUCKET-NAME."
  (interactive "sBucket Name: s3://")
  (let ((output (shell-command-to-string (concat (aws-cmd) "s3 mb s3://" bucket-name))))
    (aws-s3-lb-refresh)
    (message (s-trim output))))

(defun aws-s3-rb (bucket-name)
  "Remove given Bucket.  --force is always used.
BUCKET-NAME is the name of the Bucket to be deleted."
  (interactive "sBucket Name: s3://")
  (when (yes-or-no-p (concat
                      "Are you sure you want to delete the bucket "
                      bucket-name
                      "?"))
    (let ((output (shell-command-to-string
                   (concat (aws-cmd) "s3 rb --force s3://" bucket-name))))
      (aws-s3-lb-refresh)
      (message (s-trim output)))))

(defun aws-s3-rb-under-cursor ()
  "Remove the Bucket under the cursor in the S3 Overview."
  (interactive)
  (let ((bucket-name (tabulated-list-get-id)))
    (aws-s3-rb bucket-name)))


(transient-define-prefix aws-s3-help-popup ()
  "AWS S3 Help Menu"
  ["Actions"
   ("d" "Delete Bucket" aws-s3-rb-under-cursor)
   ("m" "Make Bucket" aws-s3-mb)
   ("P" "Set AWS Profile" aws-set-profile)
   ("q" "Services" aws)
   ("r" "Refresh Buffer" aws-s3-lb-refresh)])

(defvar aws-s3-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") 'aws-s3-help-popup)
    (define-key map (kbd "d") 'aws-s3-rb-under-cursor)
    (define-key map (kbd "m") 'aws-s3-mb)
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws)
    (define-key map (kbd "r") 'aws-s3-lb-refresh)
    map))

(defun aws-s3 ()
  "Open the S3 Mode."
  (interactive)
  (aws--pop-to-buffer (aws--buffer-name "s3"))
  (aws-s3-mode))

(define-derived-mode aws-s3-mode tabulated-list-mode "aws-s3"
  "AWS S3 mode"
  (setq major-mode 'aws-s3-mode)
  (use-local-map aws-s3-mode-map)
  (aws-s3-lb))

(provide 'aws-s3)
;;; aws-s3.el ends here
