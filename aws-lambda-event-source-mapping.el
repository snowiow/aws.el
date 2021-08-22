;;; package --- Summary
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;;; Code:
(require 'aws-core)
(require 'transient)

(defvar-local current-lambda-function-name nil)

(defun aws-lambda-list-event-source-mappings-from-line-under-cursor ()
  "List Lambda Event Sources based on the lambda name under the curser.
Used in AWS Lambda Mode."
  (interactive)
  (let ((function-name (aref (tabulated-list-get-entry) 0)))
    (aws-lambda-event-source-mapping function-name)))

(defun aws--lambda-list-event-source-mappings (function-name)
  "List Lambda Event Sources based on the lambda FUNCTION-NAME."
  (let ((rows
         (mapcar
          (lambda (x)
            (let ((splitted (split-string x "\t")))
              (list (car splitted) (vconcat (cdr splitted)))))
          (butlast
           (split-string
            (shell-command-to-string
             (concat
              (aws-cmd)
              "lambda list-event-source-mappings "
              "--function-name " function-name
              " --query 'EventSourceMappings[*].[UUID,EventSourceArn,State]' "
              "--output text")) "\n")))))
    (setq tabulated-list-format [("EventSourceArn" 100) ("State" 0)])
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))

(defun aws-lambda-get-event-source-mapping ()
  "Describe the Lambda Event Source Mapping under the cursor.
Used in AWS Lambda Event Source Mapping Mode."
  (interactive)
  (let* ((uuid (tabulated-list-get-id))
         (subcmd (concat "lambda get-event-source-mapping --uuid=" uuid))
         (buffer (concat "*" subcmd "*"))
         (cmd (concat (aws-cmd) subcmd)))
    (call-process-shell-command cmd nil buffer)
    (switch-to-buffer buffer)
    (with-current-buffer buffer (aws-view-mode))))

(defun aws-lambda-update-event-source-mapping (&optional args)
  "Update the Event Source Mapping under the cursor.
Used in AWS Lambda Event Source Mapping Mode.
Called from the aws-lambda-update-event-source-mapping-popup transient.
ARGS represent the arguments set in the transient."
  (interactive (list (transient-args 'aws-lambda-update-event-source-mapping-popup)))
  (let* ((uuid (tabulated-list-get-id))
         (subcmd
          (concat
           "lambda update-event-source-mapping "
           "--uuid=" uuid " "
           (mapconcat 'identity args " ")))
         (buffer (concat "*" subcmd "*"))
         (cmd (concat (aws-cmd) subcmd)))
    (call-process-shell-command cmd nil buffer)
    (switch-to-buffer buffer)
    (with-current-buffer buffer (aws-view-mode))))

;; TRANSIENTS
(transient-define-prefix aws-lambda-event-source-mapping-help-popup ()
  ["Actions"
   ("RET" "Get Event Source Mapping" aws--lambda-get-event-source-mapping)
   ("q" "Lambdas" aws-lambda)
   ("r" "Refresh Buffer" aws-lambda-event-source-mapping)
   ("P" "Set AWS Profile" aws-set-profile)
   ("u" "Update Event Source Mapping" aws-lambda-update-event-source-mapping-popup)])

(transient-define-prefix aws-lambda-update-event-source-mapping-popup ()
  "AWS Update Event Source Mapping Transient."
  ["Arguments"
   ("a" "[integer] (Streams) Discard records older than the specified age. The default value is infinite (-1)" "--maximum-record-age-in-seconds=")
   (aws-lambda-update-event-source-mapping-bisect)
   (aws-lambda-update-event-source-mapping-state)
   ("p" "[integer] (Streams) The number of batches to process from each shard concurrently." "--parallelization-factor=")
   ("r" "[integer] (Streams) Discard records after the specified number of retries. The default value is infinite (-1). When set to infinite (-1), failed records will be retried until the record expires." "--maximum-retry-attempts=")
   ("s" "[integer] The maximum number of items to retrieve in a single batch." "--batch-size=")
   ("t" "[integer] (Streams) The duration in seconds of a processing window. The range is between 1 second up to 900 seconds." "--tumbling-window-in-seconds=")
   ("w" "[integer] (Streams and SQS standard queues) The maximum amount of time to gather records before invoking the function, in seconds" "--maximum-batching-window-in-seconds=")]
  ["Update"
   ("u" "update event source mapping" aws-lambda-update-event-source-mapping)])

(transient-define-argument aws-lambda-update-event-source-mapping-bisect ()
  :description "[boolean] (Streams) If the function returns an error, split the batch in two and retry"
  :class 'transient-switches
  :key "b"
  :argument-format "--%s"
  :argument-regexp "\\(no-bisect-batch-on-function-error\\|bisect-batch-on-function-error\\)"
  :choices '("no-bisect-batch-on-function-error" "bisect-batch-on-function-error"))

(transient-define-argument aws-lambda-update-event-source-mapping-state ()
  :description "[boolean] Enable or Disable event source mapping"
  :class 'transient-switches
  :key "e"
  :argument-format "%s"
  :argument-regexp "\\--no-enabled\\|--enabled\\"
  :choices '("--no-enabled" "--enabled"))

;; MODE-MAP
(defvar aws-lambda-event-source-mapping-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws--lambda-get-event-source-mapping)
    (define-key map (kbd "?")   'aws-lambda-event-source-mapping-help-popup)
    (define-key map (kbd "q")   'aws-lambda)
    (define-key map (kbd "P")   'aws-set-profile)
    (define-key map (kbd "r")   'aws-lambda-event-source-mapping-refresh)
    (define-key map (kbd "u")   'aws-lambda-update-event-source-mapping)
    map))

(defun aws-lambda-event-source-mapping-refresh ()
  "Refresh the Event Source Mappings of the current Lambda Function.
Current Lambda Function is the one last chosen in AWS Lambda Mode."
  (interactive)
  (message "Refreshing buffer...")
  (aws-lambda-event-source-mapping current-lambda-function-name))

(defun aws-lambda-event-source-mapping (lambda-function)
  "Open AWS Lambda Event Source Mapping Mode.
LAMBDA-FUNCTION is the Lambda Function Name to list the event source mappings from."
  (interactive "slambda function name: ")
  (setq aws--current-service "lambda-event-source-mapping")
  (aws--pop-to-buffer (aws--buffer-name))
  (aws-lambda-event-source-mapping-mode)
  (setq-local current-lambda-function-name lambda-function)
  (aws--lambda-list-event-source-mappings current-lambda-function-name))

(define-derived-mode aws-lambda-event-source-mapping-mode tabulated-list-mode "aws-lambda-event-source-mapping"
  "AWS event source mapping"
  (setq major-mode 'aws-lambda-event-source-mapping-mode)
  (use-local-map aws-lambda-event-source-mapping-mode-map))

(provide 'aws-lambda-event-source-mapping)
;;; aws-lambda-event-source-mapping ends here
