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

(defvar aws-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-get-service)
    (define-key map (kbd "P")   'aws-set-profile)
    map))

;; TODO: move evil stuff into own minor mode to make it optional
;; (evil-set-initial-state 'aws-mode 'motion)

;; (evil-define-key 'motion aws-mode-map
;;   (kbd "RET") #'aws-get-service
;;   (kbd "P")   #'aws-set-profile)

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
