(require 'aws-core)
(require 'aws-lambda-event-source-mapping)
(require 'aws-log-streams)
(require 'aws-view)
(require 'transient)

(defun aws--lambda-list-functions ()
  "List all Lambda Functions."
  (fset 'aws--last-view 'aws-lambda)
  (aws--tabulated-list-from-command
   "lambda list-functions --output=text --query 'Functions[*].FunctionName'"
   [("Functions" 100)]))

(defun aws-lambda-get-function ()
  "Describe the Lambda Function under the cursor.
This function is used in the AWS Lambda Mode."
  (interactive)
  (let* ((buffer "*aws.el: lambda-info*")
        (function-name (aref (tabulated-list-get-entry) 0))
        (function-desc-cmd
         (concat (aws-cmd) "lambda get-function --output yaml --function-name " function-name)))
    (call-process-shell-command function-desc-cmd nil buffer)
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (aws-view-mode))))

(defun aws-lambda-get-latest-logs ()
  "Get the latest logs of the Lambda Function under the cursor.
This function is used in the AWS Lambda Mode."
  (interactive)
  (let* ((function-name (aref (tabulated-list-get-entry) 0))
         (log-group-name (concat "/aws/lambda/" function-name))
         (latest-log-stream
          (car
          (split-string
           (shell-command-to-string
            (concat
             (aws-cmd)
             (aws-log-streams-get-latest-logs-command log-group-name "1"))) "\n"))))
    (aws-log-get-log-events log-group-name latest-log-stream)))

(defun aws-lambda-describe-log-streams ()
  "List all log streams for the Lambda Function under the cursor.
This functions is used in the AWS Lambda Mode."
  (interactive)
  (let* ((function-name (aref (tabulated-list-get-entry) 0))
         (log-group-name (concat "/aws/lambda/" function-name)))
    (aws-log-streams log-group-name)))

(define-transient-command aws-lambda-help-popup ()
  "AWS Lambda Menu"
  ["Actions"
   ("RET" "Get Function" aws-lambda-get-function)
   ("e" "List Event Sources" aws-lambda-list-event-source-mappings-from-line-under-cursor)
   ("l" "Get log streams" aws-lambda-describe-log-streams)
   ("L" "Get latest logs" aws-lambda-get-latest-logs)
   ("P" "Set AWS Profile" aws-set-profile)
   ("q" "Service Overview" aws)])

(defvar aws-lambda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-lambda-get-function)
    (define-key map (kbd "?") 'aws-lambda-help-popup)
    (define-key map (kbd "e") 'aws-lambda-list-event-source-mappings-from-line-under-cursor)
    (define-key map (kbd "l") 'aws-lambda-describe-log-streams)
    (define-key map (kbd "L") 'aws-lambda-get-latest-logs)
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws)
    map))

(defun aws-lambda ()
  "Open the AWS Lambda Mode."
  (interactive)
  (setq aws--current-service "lambda")
  (aws--pop-to-buffer (aws--buffer-name))
  (aws-lambda-mode))

(define-derived-mode aws-lambda-mode tabulated-list-mode "aws-lambda"
  "AWS mode"
  (setq major-mode 'aws-lambda-mode)
  (use-local-map aws-lambda-mode-map)
  (aws--lambda-list-functions))

(provide 'aws-lambda)
;;; aws-lambda.el ends here
