;;; package --- Summary
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:
;;; Code:
(require 'aws-core)
(require 'aws-cloudwatch-alarms)

(defun aws--cloudwatch-list ()
  "List all CloudWatch services."
  (interactive)
  (let ((rows (list '("alarms" ["alarms"]))))
    (fset 'aws--last-view 'aws-cloudwatch)
    (setq tabulated-list-format [("CloudWatch" 100)])
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))

;; MODE-MAP
(defvar aws-cloudwatch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'aws-cloudwatch-alarms)
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws)
    map))

;; MODE DEFINITION
(defun aws-cloudwatch ()
  "Open AWS CloudWatch Major Mode.  This presents an overview of CloudWatch subservices."
  (interactive)
  (setq aws--current-service "cloudwatch")
  (aws--pop-to-buffer (aws--buffer-name))
  (aws-cloudwatch-mode))

(define-derived-mode aws-cloudwatch-mode tabulated-list-mode "aws-cloudwatch"
  "AWS mode"
  (setq major-mode 'aws-cloudwatch-mode)
  (use-local-map aws-cloudwatch-mode-map)
  (aws--cloudwatch-list))

(provide 'aws-cloudwatch)
;;; aws-cloudwatch.el ends here
