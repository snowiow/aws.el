;;; package --- Summary
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;;; Code:
(require 'aws-core)
(require 'transient)

; TODO: When https://github.com/aws/aws-cli/issues/6189 gets intgrated, this list can be inverted. This would end up in just one item
(defvar aws--cloudformation-stack-status-filter "CREATE_IN_PROGRESS CREATE_FAILED CREATE_COMPLETE ROLLBACK_IN_PROGRESS ROLLBACK_FAILED ROLLBACK_COMPLETE DELETE_IN_PROGRESS DELETE_FAILED UPDATE_IN_PROGRESS UPDATE_COMPLETE_CLEANUP_IN_PROGRESS UPDATE_COMPLETE UPDATE_ROLLBACK_IN_PROGRESS UPDATE_ROLLBACK_FAILED UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS UPDATE_ROLLBACK_COMPLETE REVIEW_IN_PROGRESS IMPORT_IN_PROGRESS IMPORT_COMPLETE IMPORT_ROLLBACK_IN_PROGRESS IMPORT_ROLLBACK_FAILED IMPORT_ROLLBACK_COMPLETE")

(defun aws--cloudformation-list-stacks (&optional pos)
  "List all CloudFormation Stacks.
If POS is set, jump to that line in the view."
  (interactive)
  (fset 'aws--last-view 'aws-cloudformation)
  (let* ((shell-cmd (concat
              (aws-cmd)
              "cloudformation list-stacks "
              "--query 'StackSummaries[*].[StackName,StackStatus]' "
              "--stack-status-filter " aws--cloudformation-stack-status-filter
              " --output text"))
        (rows
         (mapcar
          (lambda (x)
            (let ((splitted (split-string x "\t")))
              (list (car splitted) (vconcat splitted))))
          (butlast
           (split-string
            (shell-command-to-string shell-cmd) "\n")))))
    (setq tabulated-list-format [("Name" 80) ("Status" 0)])
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)
    (when (numberp pos)
      (goto-line pos))))

(defun aws--cloudformation-list-stacks-refresh ()
  "Refresh the CloudFormation Stacks Overview and jump to the last position."
  (interactive)
  (message "Refreshing buffer...")
  (let ((current-line (+ 1 (count-lines 1 (point)))))
    (aws--cloudformation-list-stacks current-line)))

(defun aws--cloudformation-describe-stack-events ()
  "Describe the CloudFormation Stack Events of the Stack under the cursor."
  (interactive)
  (aws--describe-current-resource "cloudformation describe-stack-events --stack-name"))

(defun aws--cloudformation-delete-stack ()
  "Delete the CloudFormation Stack under the cursor."
  (interactive)
  (let ((stack-name (tabulated-list-get-id)))
    (when (y-or-n-p (concat
                     "Are you sure you want to delete the stack "
                     stack-name
                     "?"))
      (let* ((sub-cmd (concat
                       "cloudformation delete-stack "
                       "--stack-name " stack-name))
             (cmd (concat (aws-cmd) sub-cmd)))
        (shell-command cmd)
        (aws--cloudformation-list-stacks-refresh)
        (message (concat "Triggered deletion on stack " stack-name))))))

(transient-define-prefix aws-cloudformation-help-popup ()
  "AWS CloudFormation Menu"
  ["Actions"
   ("d" "Delete Stack" aws--cloudformation-delete-stack)
   ("e" "Describe Stack Events" aws--cloudformation-describe-stack-events)
   ("P" "Set AWS Profile" aws-set-profile)
   ("q" "Service Overview" aws)
   ("r" "Refresh Buffer" aws--cloudformation-list-stacks-refresh)])

(defvar aws-cloudformation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") 'aws-cloudformation-help-popup)
    (define-key map (kbd "d") 'aws--cloudformation-delete-stack)
    (define-key map (kbd "e") 'aws--cloudformation-describe-stack-events)
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws)
    (define-key map (kbd "r") 'aws--cloudformation-list-stacks-refresh)
    map))

(defun aws-cloudformation ()
  "Open the CloudFormation Mode."
  (interactive)
  (setq aws--current-service "cloudformation")
  (aws--pop-to-buffer (aws--buffer-name))
  (aws-cloudformation-mode))

(define-derived-mode aws-cloudformation-mode tabulated-list-mode "aws-cloudformation"
  "AWS CloudFormation mode"
  (setq major-mode 'aws-cloudformation-mode)
  (use-local-map aws-cloudformation-mode-map)
  (aws--cloudformation-list-stacks))

(provide 'aws-cloudformation)
;;; aws-cloudformation.el ends here
