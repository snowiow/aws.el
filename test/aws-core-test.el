(require 'ert)
(require 'aws-core)


(defun aws-core--test-without-arg ()
  (setq-local aws-core--test-without-arg-was-called t))

(defun aws-core--test-with-arg ()
  (setq-local aws-core--test-with-arg-was-called t))

(ert-deftest aws-core--test-refresh-list-view ()
  (setq-local aws-core--test-without-arg-was-called nil)
  (setq-local aws-core--test-with-arg-was-called nil)
  (should (null aws-core--test-without-arg-was-called))
  (should (null aws-core--test-with-arg-was-called))
  (aws-core--refresh-list-view 'aws-core--test-without-arg)
  (should (equal aws-core--test-without-arg-was-called t))
  (should (null aws-core--test-with-arg-was-called))
  (aws-core--refresh-list-view 'aws-core--test-with-arg)
  (should (equal aws-core--test-with-arg-was-called t)))

(ert-deftest aws-core--test-get-current-line ()
  (cl-letf (((symbol-function 'count-lines)
             (lambda (start end) 12)))
    (should (equal 13 (aws-core--get-current-line)))))

(provide 'aws-core-test)
;;; aws-core-test.el ends here
