;;; package --- Summary

;;; Commentary:

;;; Code:
(require 'ert)
(require 'aws-mode)

(ert-deftest aws--test-buffer-name ()
  (let ((aws-profile "default"))
    (should (equal "*aws.el [profile: default] [service: lambda]*" (aws--buffer-name "lambda")))))

(provide 'aws-core-test)
;;; aws-test.el ends here
