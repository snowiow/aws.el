(defvar aws-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'kill-current-buffer)
    map))

;; (evil-set-initial-state 'aws-view-mode 'motion)

;; (evil-define-key 'motion aws-view-mode-map
;;   (kbd "q") #'kill-current-buffer)

(define-derived-mode aws-view-mode fundamental-mode "aws view"
  "AWS mode"
  (setq major-mode 'aws-view-mode)
  (use-local-map aws-view-mode-map)
  (view-mode))

(provide 'aws-view)
