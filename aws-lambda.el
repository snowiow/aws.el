(require 'evil)
(require 'aws-core)
(require 'aws-view)
(require 'transient)

(defun aws-lambda-list-functions ()
  (fset 'aws-last-view 'aws-lambda)
  (aws--tabulated-list-from-command
   "lambda list-functions --output=text --query 'Functions[*].FunctionName'"
   [("Functions" 100)]))

(defun aws-lambda-get-function ()
  (interactive)
  (let* ((buffer "*lambda-info*")
        (function-name (aref (tabulated-list-get-entry) 0))
        (function-desc-cmd
         (concat (aws-cmd) "lambda get-function --output yaml --function-name " function-name)))
    (call-process-shell-command function-desc-cmd nil buffer)
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (aws-view-mode))))

(defun aws-lambda-get-latest-logs ()
  (interactive)
  (let* ((buffer "*lambda-info*")
         (function-name (aref (tabulated-list-get-entry) 0))
         (log-group-name (concat "/aws/lambda/" function-name))
         (latest-log-stream
          (car 
          (split-string
           (shell-command-to-string
            (concat
             (aws-cmd)
             "logs describe-log-streams --log-group-name '"
             log-group-name
             "' --max-items 1 --order-by LastEventTime --descending --query logStreams[].logStreamName --output text")) "\n")))
          (get-logs-cmd
           (concat (aws-cmd) "logs get-log-events --log-group-name '" log-group-name "' --log-stream-name '" latest-log-stream "'")))
    (call-process-shell-command get-logs-cmd nil buffer)
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (aws-view-mode))))

(define-transient-command aws-lambda-help-popup ()
  "AWS Lambda Menu"
  ["Actions"
   ("RET" "Get Function" aws-lambda-get-function)
   ("q" "Service Overview" aws)
   ("L" "Get latest logs" aws-lambda-get-latest-logs)])

(defvar aws-lambda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-lambda-get-function)
    (define-key map (kbd "q") 'aws)
    (define-key map (kbd "?") 'aws-lambda-help-popup)
    (define-key map (kbd "L") 'aws-lambda-get-latest-logs)
    (define-key map (kbd "P") 'aws-set-profile)
    map))

;; (evil-set-initial-state 'aws-lambda-mode 'motion)

;; (evil-define-key 'motion aws-lambda-mode-map
;;   (kbd "RET") #'aws-lambda-get-function
;;   (kbd "q") #'aws
;;   (kbd "?")  #'aws-lambda-help-popup
;;   (kbd "L")   #'aws-lambda-get-latest-logs
;;   (kbd "P")   #'aws-set-profile)

(defun aws-lambda ()
  (interactive)
  (setq aws-current-service "lambda")
  (aws--pop-to-buffer (aws--buffer-name))
  (aws-lambda-mode))

(define-derived-mode aws-lambda-mode tabulated-list-mode "aws lambda"
  "AWS mode"
  (setq major-mode 'aws-lambda-mode)
  (use-local-map aws-lambda-mode-map)
  (aws-lambda-list-functions))

(provide 'aws-lambda)
