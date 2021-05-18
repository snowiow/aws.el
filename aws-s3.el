(require 'evil)
(require 'aws-core)

(defun aws-s3-lb ()
  (let ((rows (mapcar (lambda (x) `(nil [,x]))
                          (split-string
                           (shell-command-to-string
                            (concat (aws-cmd) "s3 ls | awk '{print $3}'")) "\n"))))
    (fset 'aws-last-view 'aws-s3)
    (setq tabulated-list-format [("Buckets" 100)])
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))

(defvar aws-s3-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "P") 'aws-set-profile)
    (define-key map (kbd "q") 'aws)
    map))

;; (evil-set-initial-state 'aws-s3-mode 'motion)

;; (evil-define-key 'motion aws-s3-mode-map
;;   (kbd "P")   #'aws-set-profile
;;   (kbd "q")   #'aws)

(defun aws-s3 ()
  (interactive)
  (setq aws-current-service "s3")
  (aws--pop-to-buffer (aws--buffer-name))
  (aws-s3-mode))

(define-derived-mode aws-s3-mode tabulated-list-mode "aws s3"
  "AWS mode"
  (setq major-mode 'aws-s3-mode)
  (use-local-map aws-s3-mode-map)
  (aws-s3-lb))

(provide 'aws-s3)
