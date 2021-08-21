(require 'aws-core)
(require 'aws-lambda-event-source-mapping)
(require 'aws-log-streams)
(require 'aws-view)
(require 'transient)

;;; Code:
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

(defun aws-lambda-invoke (&optional args)
  "Invoke the Lambda Function under the cursor.
ARGS represent the arguments set in the transient."
  (interactive (list (transient-args 'aws-lambda-invoke-popup)))
  (let* ((function-name (aref (tabulated-list-get-entry) 0))
         (outfile-path (lambda-tmp-outfile function-name))
         (subcmd
          (concat
           "lambda invoke --function-name "
           function-name " "
           (mapconcat 'identity args " ")
           " " outfile-path))
         (buffer (concat "*" subcmd "*"))
         (cmd (concat (aws-cmd) subcmd)))
    (call-process-shell-command cmd nil buffer)
    (switch-to-buffer buffer)
    (with-current-buffer buffer (aws-view-mode))))

(defun lambda-tmp-outfile (function-name)
  (concat "/tmp/aws-el-lambda-" function-name "-output.json"))

(defun aws-lambda-view-last-execution ()
  (interactive)
  (let* ((function-name (aref (tabulated-list-get-entry) 0))
         (outfile-path (lambda-tmp-outfile function-name)))
    (switch-to-buffer (find-file-other-window outfile-path))))

;; TRANSIENTS
(define-transient-command aws-lambda-help-popup ()
  "AWS Lambda Menu"
  ["Actions"
   ("RET" "Get Function" aws-lambda-get-function)
   ("e" "List Event Sources" aws-lambda-list-event-source-mappings-from-line-under-cursor)
   ("i" "Invoke Function" aws-lambda-invoke-popup)
   ("l" "Get log streams" aws-lambda-describe-log-streams)
   ("L" "Get latest logs" aws-lambda-get-latest-logs)
   ("o" "View oufile from functions last execution" aws-lambda-view-last-execution)
   ("P" "Set AWS Profile" aws-set-profile)
   ("q" "Service Overview" aws)])

(transient-define-prefix aws-lambda-invoke-popup ()
  "AWS Lambda Invoke Transient"
  ["Arguments"
   ("c" "[string] Up to 3583 bytes of base64 encoded data about the invoking client to pass to the function in the context object." "--client-context=")
   (aws-lambda-invoke-log-type)
   ("p" "[string] The JSON that you want to provide to your Lambda function as input." "--payload=")
   ("q" "[string] Specify a version or alias to invoke a published version of the function." "--qualifier=")
   (aws-lambda-invoke-invocation-type)]
  ["Invoke"
   ("i" "invoke function" aws-lambda-invoke)])

(transient-define-argument aws-lambda-invoke-invocation-type ()
  :description "Invocation Type"
  :class 'transient-switches
  :key "t"
  :argument-format "--invocation-type=%s"
  :argument-regexp "\\(RequestResponse\\|Event\\|DryRun\\)"
  :choices '("RequestResponse" "Event" "DryRun"))

(transient-define-argument aws-lambda-invoke-log-type ()
  :description "Set to Tail to include the execution logs in the response"
  :class 'transient-switches
  :key "l"
  :argument-format "--log-type=%s"
  :argument-regexp "\\(None\\|Tail\\)"
  :choices '("None" "Tail"))

(defvar aws-lambda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-lambda-get-function)
    (define-key map (kbd "?") 'aws-lambda-help-popup)
    (define-key map (kbd "e") 'aws-lambda-list-event-source-mappings-from-line-under-cursor)
    (define-key map (kbd "i") 'aws-lambda-invoke-popup)
    (define-key map (kbd "l") 'aws-lambda-describe-log-streams)
    (define-key map (kbd "L") 'aws-lambda-get-latest-logs)
    (define-key map (kbd "o") 'aws-lambda-view-last-execution)
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
  "AWS Lambda mode"
  (setq major-mode 'aws-lambda-mode)
  (use-local-map aws-lambda-mode-map)
  (aws--lambda-list-functions))

(provide 'aws-lambda)
;;; aws-lambda.el ends here
