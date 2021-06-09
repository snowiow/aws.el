(require 'aws-core)

(defun aws--cloudwatch-describe-alarms ()
  ())

(defvar aws-cloudwatch-alarms-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map ("P") 'aws-set-profile)
    (define-key map ("q") 'aws-cloudwatch)))

(defun aws-cloudwatch-alarms ()
  (interactive)
  (setq aws--current-service "cloudwatch-alarms")
  (aws--pop-to-buffer (aws--buffer-name))
  (aws-cloudwatch-alarms-mode))

(define-derived-mode aws-cloudwatch-alarms-mode tabulated-list-mode "aws cloudwatch alarms"
  "AWS CloudWatch Alarms Mode"
  (setq major-mode 'aws-cloudwatch-alarms-mode)
  (use-local-map aws-cloudwatch-alarms-mode-map))

(provide 'aws-cloudwatch-alarms)
;;; aws-cloudwatch-alarms.el ends here
