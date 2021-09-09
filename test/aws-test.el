;;; package --- Summary

;;; Commentary:

;;; Code:
(require 'ert)
(require 'aws-mode)

(ert-deftest aws--test-buffer-name ()
  (let ((aws-profile "default")
        (aws--current-service "lambda"))
    (should (equal "*aws.el [profile: default] [service: lambda]*" (aws--buffer-name)))))

(provide 'aws-core-test)
;;; aws-test.el ends here
