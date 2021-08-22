;;; package --- Summary
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;;; Code:
(defun longest-element (list)
  "Return the longest element in a LIST of strings.
Used to determine the width of a column in tabulated list view."
  (apply 'max (mapcar (lambda (x) (length x)) list)))

(provide 'aws-utils)
;;; aws-utils.el ends here
