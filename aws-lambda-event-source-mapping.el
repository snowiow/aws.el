(require 'aws-core)

(defvar-local current-lambda-function-name nil)

(defun aws-lambda-list-event-source-mappings-from-line-under-cursor ()
  (interactive)
  (let ((function-name (aref (tabulated-list-get-entry) 0)))
    (aws-lambda-event-source-mapping function-name)))

(defun aws-lambda-list-event-source-mappings (function-name)
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

(defvar aws-lambda-event-source-mapping-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'aws-lambda)
    (define-key map (kbd "P") 'aws-set-profile)
    map))

(defun aws-lambda-event-source-mapping (lambda-function)
  (interactive "slambda function name: ")
  (setq aws-current-service "lambda-event-source-mapping")
  (aws--pop-to-buffer (aws--buffer-name))
  (aws-lambda-event-source-mapping-mode)
  (setq-local current-lambda-function-name lambda-function)
  (aws-lambda-list-event-source-mappings current-lambda-function-name))

(define-derived-mode aws-lambda-event-source-mapping-mode tabulated-list-mode "aws-event-source-mapping"
  "AWS event source mapping"
  (setq major-mode 'aws-lambda-event-source-mapping-mode)
  (use-local-map aws-lambda-event-source-mapping-mode-map))

(provide 'aws-lambda-event-source-mapping)
