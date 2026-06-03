;;; package --- Summary

;;; Commentary:

;;; Code:
(require 'ert)
(require 'aws)

(ert-deftest aws--test-buffer-name ()
  (let ((aws-profile "default")
        (aws-region "eu-central-1"))
    (should (equal "*aws.el [profile: default] [region: eu-central-1] [service: lambda]*"
                   (aws--buffer-name "lambda")))))

(ert-deftest aws--test-cmd-includes-region ()
  (let ((aws-profile "default")
        (aws-region "eu-central-1")
        (aws-login-method 'profile))
    (should (equal "aws --profile default --region eu-central-1 "
                   (aws-cmd)))))

(provide 'aws-core-test)
;;; aws-test.el ends here
