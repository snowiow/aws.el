;;; package --- Summary
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;;; Code:
(require 'aws-view)

(defvar aws--current-service nil)

(defvar aws-profile (car
                     (split-string
                      (shell-command-to-string "aws configure list-profiles") "\n")))

(defcustom aws-vault nil
  "Set if aws-vault should be used for aws sessions."
  :type 'symbol
  :group 'aws
  :options '('t 'nil))

(fset 'aws--last-view nil)

(defun aws--buffer-name ()
  "Return aws.el buffer name."
  (concat (format "*aws.el [profile: %s] [service: %s]" aws-profile aws--current-service) "*"))

(defun aws--pop-to-buffer (name)
  "Create a buffer with NAME if not exists and switch to it."
  (unless (get-buffer name)
    (get-buffer-create name))
    (pop-to-buffer-same-window name))

(defun aws-cmd ()
  "Create the AWS base cmd with the right profile.
Use either aws-vault exec or --profile based on setting."
  (if aws-vault
      (concat "aws-vault exec "
              aws-profile
              " -- aws ")
    (concat "aws --profile " aws-profile)))

(defun aws-set-profile ()
  "Set active AWS Profile."
  (interactive)
  (setq aws-profile
        (completing-read
         "Select profile: "
         (split-string (shell-command-to-string "aws configure list-profiles") "\n")))
  (aws--last-view))

(defun aws--tabulated-list-from-command (cmd header)
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

(defun aws--describe-current-resource (cmd)
  "Describe resource under cursor.  CMD is the aws command to describe the resource."
  (let* ((current-resource (tabulated-list-get-id))
         (buffer (concat "*aws.el [" aws--current-service "]: " current-resource "*"))
         (cmd (concat (aws-cmd) "--output yaml " cmd " " current-resource)))
    (call-process-shell-command cmd nil buffer)
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (aws-view-mode))))

(provide 'aws-core)
;;; aws-core.el ends here
