(require 'aws-core)

;;; Code:
; TODO: When https://github.com/aws/aws-cli/issues/6189 gets intgrated, this list can be inverted. This would end up in just one item
(defvar aws--cloudformation-stack-status-filter "CREATE_IN_PROGRESS CREATE_FAILED CREATE_COMPLETE ROLLBACK_IN_PROGRESS ROLLBACK_FAILED ROLLBACK_COMPLETE DELETE_IN_PROGRESS DELETE_FAILED UPDATE_IN_PROGRESS UPDATE_COMPLETE_CLEANUP_IN_PROGRESS UPDATE_COMPLETE UPDATE_ROLLBACK_IN_PROGRESS UPDATE_ROLLBACK_FAILED UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS UPDATE_ROLLBACK_COMPLETE REVIEW_IN_PROGRESS IMPORT_IN_PROGRESS IMPORT_COMPLETE IMPORT_ROLLBACK_IN_PROGRESS IMPORT_ROLLBACK_FAILED IMPORT_ROLLBACK_COMPLETE")

(defun aws--cloudformation-list-stacks ()
  "List all CloudFormation Stacks."
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
    (hl-line-mode 1)))

(defvar aws-cloudformation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") 'aws-cloudformation-help-popup)
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws)
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
