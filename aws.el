(require 'aws-cloudwatch)
(require 'aws-core)
(require 'aws-lambda)
(require 'aws-logs)
(require 'aws-s3)

(defun aws-list-services ()
  "List available aws services."
  (interactive)
  (let ((rows (list '("cloudwatch" ["cloudwatch"])
                    '("lambda" ["lambda"])
                    '("logs" ["logs"])
                    '("s3" ["s3"]))))
    (fset 'aws-last-view 'aws)
    (setq tabulated-list-format [("Services" 100)])
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))

(defun aws-get-service ()
  (interactive)
  (let ((service (if (tabulated-list-get-entry)
                     (aref (tabulated-list-get-entry) 0)
                   aws-current-service)))
    (cond ((equal service "cloudwatch") (aws-cloudwatch))
          ((equal service "services") (aws))
          ((equal service "lambda") (aws-lambda))
          ((equal service "logs") (aws-logs))
          ((equal service "s3") (aws-s3))
          (t (message "Hello")))))

(defun aws-quit ()
  "Quits the aws major mode by killing all it's open buffers."
  (interactive)
  (when (y-or-n-p "Are you sure you want to quit? ")
    (let* ((buffer-names (mapcar (lambda (x) (buffer-name x)) (buffer-list)))
           (aws-buffers (seq-filter
                         (lambda (x) (string-match-p "\\*aws\\.el\s.+\\*" x))
                         buffer-names)))
      (dolist (buffer-name aws-buffers)
        (kill-buffer buffer-name)))))

;; MODE-MAP
(defvar aws-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-get-service)
    (define-key map (kbd "P")   'aws-set-profile)
    (define-key map (kbd "q")   'aws-quit)
    map))

;; MODE DEFINITION
(defun aws ()
  (interactive)
  (setq aws-current-service "services")
  (aws--pop-to-buffer (aws--buffer-name))
  (aws-mode))

(define-derived-mode aws-mode tabulated-list-mode "aws"
  "AWS mode"
  (setq major-mode 'aws-mode)
  (use-local-map aws-mode-map)
  (aws-list-services))

(provide 'aws)
;;; aws.el ends here
