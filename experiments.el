(defun aws-start-process ()
  (let ((process (start-process "my-process" "*my-buffer*" "aws-vault" "exec" "trip-dev"))
        )
    (set-process-filter process #'aws-process-filter)))

(defun aws-process-filter (proc string)
  (aws-process-otp-prompt proc string))

(defun aws-process-otp-prompt (process string)
  (process-send-string process (read-passwd string)))

(process-list)
