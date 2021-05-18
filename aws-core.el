(defvar aws-current-service nil)
(defvar aws-profile (car
                     (split-string
                      (shell-command-to-string "aws configure list-profiles") "\n")))

(defcustom aws-vault nil
  "Set if aws-vault should be used for aws sessions"
  :type 'symbol
  :group 'aws
  :options '('t 'nil))

(fset 'aws-last-view nil)

(defun aws--buffer-name ()
  "Return aws.el buffer name."
  (concat (format "*aws.el [profile: %s] [service: %s]" aws-profile aws-current-service) "*"))

(defun aws--pop-to-buffer (name)
  (unless (get-buffer name)
    (get-buffer-create name))
    (pop-to-buffer-same-window name))

(defun aws-cmd ()
  (if aws-vault
      (concat "aws-vault exec "
              aws-profile
              " -- aws ")
    "aws "))

(defun aws-set-profile ()
  "Set active AWS Profile."
  (interactive)
  (setq aws-profile
        (completing-read
         "Select profile: "
         (split-string (shell-command-to-string "aws configure list-profiles") "\n")))
  (aws-last-view))

(defun aws--tabulated-list-from-command (cmd header)
  (let ((rows
         (mapcar
          (lambda (x) `(nil [,x]))
          (split-string
           (shell-command-to-string
            (concat (aws-cmd) cmd)) "\t"))))
    (setq tabulated-list-format header)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (hl-line-mode 1)))

(provide 'aws-core)
;;; aws.el ends here
