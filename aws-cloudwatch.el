(require 'aws-core)

(defun aws-cloudwatch-list ()
  (interactive)
  (let ((rows (list '("alarms" ["alarms"]))))
    (fset 'aws-last-view 'aws-cloudwatch)
    (setq tabulated-list-format [("CloudWatch" 100)])
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))

(defvar aws-cloudwatch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws)
    map))

(defun aws-cloudwatch ()
  (interactive)
  (setq aws-current-service "cloudwatch")
  (aws--pop-to-buffer (aws--buffer-name))
  (aws-cloudwatch-mode))

(define-derived-mode aws-cloudwatch-mode tabulated-list-mode "aws cloudwatch"
  "AWS mode"
  (setq major-mode 'aws-cloudwatch-mode)
  (use-local-map aws-cloudwatch-mode-map)
  (aws-cloudwatch-list))

(provide 'aws-cloudwatch)
