(require 'evil)
(require 'aws)
(require 'aws-core)
(require 'aws-s3)
(require 'aws-lambda)
(require 'aws-view)

(defgroup aws-evil nil
  "Provides integration of aws and evil."
  :group 'aws
  :prefix "aws-evil-")

(defvar aws-evil-mode-map (make-sparse-keymap))

;;; Code:
(define-minor-mode aws-evil-mode
  "Brings evil keybindings to aws"
  :lighter "aws-evil"
  :keymap aws-evil-mode-map
  :group 'aws-evil)

;; aws-mode
(evil-define-key 'normal aws-mode-map
  (kbd "RET") #'aws-get-service
  (kbd "P")   #'aws-set-profile)

;; aws-cloudwatch-mode
(evil-define-key 'normal aws-cloudwatch-mode-map
  (kbd "P")   #'aws-set-profile
  (kbd "q")   #'aws)

;; aws-lambda-mode
(evil-define-key 'normal aws-lambda-mode-map
  (kbd "RET") #'aws-lambda-get-function
  (kbd "q") #'aws
  (kbd "?")  #'aws-lambda-help-popup
  (kbd "L")   #'aws-lambda-get-latest-logs
  (kbd "P")   #'aws-set-profile)

;; aws-logs-mode
(evil-define-key 'normal aws-logs-mode-map
  (kbd "RET") #'aws-log-streams-from-line-under-cursor
  (kbd "P")   #'aws-set-profile
  (kbd "q")   #'aws)

;; aws-log-streams
(evil-define-key 'normal aws-log-streams-mode-map
  (kbd "RET") #'aws-log-get-log-events-in-log-streams-view
  (kbd "P")   #'aws-set-profile
  (kbd "q")   #'aws-logs)

;; aws-s3-mode
(evil-define-key 'normal aws-s3-mode-map
  (kbd "P")   #'aws-set-profile
  (kbd "q")   #'aws)

;; aws-view-mode
(evil-define-key 'normal aws-view-mode-map
  (kbd "q") #'kill-current-buffer)

(provide 'aws-evil)

;;; aws-evil.el ends here
