;;; package --- Summary
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;;; Code:
(defvar aws-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'kill-current-buffer)
    map))

(define-derived-mode aws-view-mode fundamental-mode "aws-view"
  "AWS mode"
  (setq major-mode 'aws-view-mode)
  (use-local-map aws-view-mode-map)
  (view-mode))

(provide 'aws-view)
;;; aws-view.el ends here
