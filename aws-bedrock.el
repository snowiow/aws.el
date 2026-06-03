;;; aws-bedrock.el --- Emacs major modes wrapping the AWS CLI  -*- lexical-binding: t; -*-

;; Copyright (C) 2022, Marcel Patzwahl

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Author: Marcel Patzwahl

;;; Commentary:

;; Emacs major modes wrapping the AWS CLI for Bedrock chat

;;; Code:
(require 'json)
(require 'markdown-mode)

(declare-function aws-cmd "aws-mode")

(defcustom aws-bedrock-model "eu.anthropic.claude-sonnet-4-5-20250929-v1:0"
  "AWS Bedrock model to use for chat interactions.
Common model IDs:
  - eu.anthropic.claude-sonnet-4-5-20250929-v1:0 (Claude Sonnet 4.5 EU)
  - anthropic.claude-sonnet-4-5-20250929-v1:0 (Claude Sonnet 4.5)
  - anthropic.claude-sonnet-4-20250514-v1:0 (Claude Sonnet 4)
  - anthropic.claude-3-7-sonnet-20250219-v1:0 (Claude 3.7 Sonnet)
  - anthropic.claude-3-5-sonnet-20240620-v1:0 (Claude 3.5 Sonnet)
  - anthropic.claude-3-sonnet-20240229-v1:0 (Claude 3 Sonnet)
  - anthropic.claude-3-haiku-20240307-v1:0 (Claude 3 Haiku)"
  :type 'string
  :group 'aws)

(defvar-local aws-bedrock--conversation-history nil
  "Conversation history for the current Bedrock chat session.")

(defvar-local aws-bedrock--input-marker nil
  "Marker for the start of the input area.")

(defvar-local aws-bedrock--output-marker nil
  "Marker for the start of the output area.")

(defun aws-bedrock--create-message-payload (user-message)
  "Create the JSON payload for Bedrock API with USER-MESSAGE."
  (let* ((messages (append aws-bedrock--conversation-history
                           (list `((role . "user")
                                   (content . ,user-message)))))
         (payload `((anthropic_version . "bedrock-2023-05-31")
                    (max_tokens . 4096)
                    (messages . ,(vconcat messages)))))
    (json-encode payload)))

(defun aws-bedrock--parse-response (response-json)
  "Parse the Bedrock API RESPONSE-JSON and extract the assistant's message."
  (condition-case err
      (let* ((response (json-read-from-string response-json))
             (content (cdr (assoc 'content response)))
             (first-content (aref content 0))
             (text (cdr (assoc 'text first-content))))
        text)
    (error
     (format "Error parsing response: %s\nResponse: %s" err response-json))))

(defun aws-bedrock--send-message (message)
  "Send MESSAGE to AWS Bedrock and display the response."
  (let* ((payload (aws-bedrock--create-message-payload message))
         (temp-file (make-temp-file "bedrock-payload" nil ".json"))
         (output-file (make-temp-file "bedrock-output" nil ".json"))
         (cmd (concat (aws-cmd)
                      "bedrock-runtime invoke-model "
                      "--model-id " aws-bedrock-model " "
                      "--body fileb://" temp-file " "
                      output-file))
         (buffer (current-buffer)))

    ;; Write payload to temp file
    (with-temp-file temp-file
      (insert payload))

    ;; Show user message
    (aws-bedrock--insert-message "Input" message)

    ;; Show loading indicator
    (let ((loading-pos (aws-bedrock--insert-message "Bedrock" "Generating response...")))

      ;; Execute AWS CLI command asynchronously
      (let ((stderr-buffer (generate-new-buffer " *bedrock-stderr*"))
            (stdout-buffer (generate-new-buffer " *bedrock-stdout*")))
        (let ((proc (make-process
                     :name "bedrock-invoke"
                     :buffer stdout-buffer
                     :command (list "/bin/bash" "-c" cmd)
                     :connection-type 'pipe
                     :stderr stderr-buffer
                     :sentinel
                     (lambda (proc event)
                       (when (memq (process-status proc) '(exit signal))
                         (let ((exit-status (process-exit-status proc)))
                           (unwind-protect
                               (when (buffer-live-p buffer)
                                 (with-current-buffer buffer
                                   (let ((inhibit-read-only t))
                                     (save-excursion
                                       ;; Remove loading indicator
                                       (goto-char loading-pos)
                                       (delete-region loading-pos (point-max))

                                       (if (and (zerop exit-status)
                                                (file-exists-p output-file)
                                                (> (nth 7 (file-attributes output-file)) 0))
                                           (let* ((response-json (with-temp-buffer
                                                                   (insert-file-contents output-file)
                                                                   (buffer-string)))
                                                  (assistant-message (aws-bedrock--parse-response response-json)))

                                             ;; Update conversation history
                                             (setq aws-bedrock--conversation-history
                                                   (append aws-bedrock--conversation-history
                                                           (list `((role . "user") (content . ,message))
                                                                 `((role . "assistant") (content . ,assistant-message)))))

                                             ;; Display assistant message
                                             (aws-bedrock--insert-message "Bedrock" assistant-message)

                                             ;; Add input separator
                                             (goto-char aws-bedrock--output-marker)
                                             (insert (propertize "\n" 'read-only t))
                                             (insert (propertize (make-string 80 ?-) 'face 'shadow 'read-only t))
                                             (insert (propertize "\n" 'read-only t))
                                             (insert (propertize "Input (C-<return> to send): " 'face 'italic 'read-only t))
                                             ;; Set input marker - everything after this point is writable
                                             (set-marker aws-bedrock--input-marker (point))
                                             ;; Insert a plain space with NO properties at all
                                             (let ((pos (point)))
                                               (insert " ")
                                               (set-text-properties pos (point) nil)))

                                         ;; Handle error
                                         (let* ((error-output (when (buffer-live-p stderr-buffer)
                                                                (with-current-buffer stderr-buffer
                                                                  (buffer-string))))
                                                (error-msg (format "Failed to invoke model.\nExit status: %s\nCommand: %s\n\nError output:\n%s\n\nOutput file exists: %s"
                                                                   exit-status
                                                                   cmd
                                                                   (if (or (not error-output) (string-empty-p error-output))
                                                                       "(no error output)"
                                                                     error-output)
                                                                   (file-exists-p output-file))))
                                           (aws-bedrock--insert-message "Error" error-msg))))

                                     ;; Move cursor to input area (outside save-excursion)
                                     (goto-char (point-max)))

                                   ;; Ensure buffer is writable after inserting response
                                   (setq buffer-read-only nil)

                                   ;; Force redisplay to show the response
                                   (redisplay t)

                                   ;; Show completion message
                                   (message "Bedrock response received")))

                             ;; Clean up temp files and buffers
                             (when (file-exists-p temp-file)
                               (delete-file temp-file))
                             (when (file-exists-p output-file)
                               (delete-file output-file))
                             (when (buffer-live-p stderr-buffer)
                               (kill-buffer stderr-buffer))
                             (when (buffer-live-p stdout-buffer)
                               (kill-buffer stdout-buffer)))))))))
          ;; Close stdin to signal no more input
          (process-send-eof proc))))))

(defun aws-bedrock--insert-message (role message)
  "Insert a MESSAGE with ROLE into the chat buffer.
Returns the position where the message starts."
  (let ((inhibit-read-only t)
        (start-pos nil))
    (save-excursion
      (goto-char aws-bedrock--output-marker)
      (unless (= (point) (point-min))
        (insert (propertize "\n" 'read-only t)))
      (setq start-pos (point))
      (insert (propertize (concat role ": ") 'face 'bold 'read-only t))
      (insert (propertize message 'read-only t))
      (insert (propertize "\n" 'read-only t))
      (set-marker aws-bedrock--output-marker (point)))
    start-pos))

(defun aws-bedrock-send-input ()
  "Send the current input to Bedrock."
  (interactive)
  (let ((input-start (marker-position aws-bedrock--input-marker))
        (input-end (point-max)))
    (when (> input-end input-start)
      (let ((message (string-trim (buffer-substring-no-properties input-start input-end))))
        (unless (string-empty-p message)
          ;; Clear input area
          (let ((inhibit-read-only t))
            (delete-region input-start input-end))

          ;; Send message
          (aws-bedrock--send-message message)

          ;; Reset input marker
          (goto-char (point-max)))))))

(defun aws-bedrock-clear-chat ()
  "Clear the chat history and start a new conversation."
  (interactive)
  (when (y-or-n-p "Clear chat history? ")
    (setq aws-bedrock--conversation-history nil)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (aws-bedrock--setup-buffer))))

(defun aws-bedrock-insert-mode ()
  "Enter insert mode at the input area for typing messages.
For evil-mode users, this switches to insert state.
For non-evil users, this just moves to the input area."
  (interactive)
  (goto-char (point-max))
  (if (and (fboundp 'evil-insert-state)
           (boundp 'evil-mode)
           evil-mode)
      (evil-insert-state)))

(defun aws-bedrock--setup-buffer ()
  "Setup the Bedrock chat buffer with appropriate markers and text."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "AWS Bedrock Chat\n" 'face 'bold 'read-only t))
    (insert (propertize (format "Model: %s\n" aws-bedrock-model) 'face 'italic 'read-only t))
    (insert (propertize "Press C-<return> to send messages\n" 'face 'italic 'read-only t))
    (insert (propertize (make-string 80 ?-) 'face 'shadow 'read-only t))
    (insert (propertize "\n\n" 'read-only t))

    ;; Set output marker
    (setq aws-bedrock--output-marker (point-marker))
    (set-marker-insertion-type aws-bedrock--output-marker nil)

    ;; Add input section
    (goto-char (point-max))
    (insert (propertize "\n" 'read-only t))
    (insert (propertize (make-string 80 ?-) 'face 'shadow 'read-only t))
    (insert (propertize "\n" 'read-only t))
    (insert (propertize "Input (C-<return> to send): " 'face 'italic 'read-only t))

    ;; Set input marker - everything after this point is writable
    (setq aws-bedrock--input-marker (point-marker))
    (set-marker-insertion-type aws-bedrock--input-marker nil)

    ;; Insert a plain space with NO properties at all
    (let ((pos (point)))
      (insert " ")
      (set-text-properties pos (point) nil))

    (goto-char (point-max))))

(transient-define-prefix aws-bedrock-help-popup ()
  "AWS Bedrock Help Menu"
  ["Actions"
   ("C-<return>" "Send Message" aws-bedrock-send-input)]
  ["Quit"
   ("C-g" "Close" transient-quit-one)])

(defun aws-bedrock--protect-input-area (beg end)
  "Prevent deletion before marker, but allow insertions."
  (when (and aws-bedrock--input-marker
             (not inhibit-read-only))  ; Don't interfere when inhibit-read-only is set
    (let ((marker-pos (marker-position aws-bedrock--input-marker)))
      ;; Only prevent deletions that affect content before the marker
      ;; Insertions (where beg=end) are always allowed
      (when (and (not (= beg end))  ; It's a deletion/replacement, not insertion
                 (< beg marker-pos))  ; And it's before the marker
        (signal 'text-read-only (list "Cannot edit before input area"))))))

(defun aws-bedrock--restore-writable-space (beg end len)
  "Automatically restore the protective space if it was deleted."
  (when (and aws-bedrock--input-marker
             (>= beg (marker-position aws-bedrock--input-marker))
             (<= (point-max) (marker-position aws-bedrock--input-marker)))
    ;; The protective space was deleted, restore it
    (let ((inhibit-modification-hooks t)
          (inhibit-read-only t))
      (goto-char aws-bedrock--input-marker)
      (let ((pos (point)))
        (insert " ")
        (set-text-properties pos (point) nil)))))

(defvar aws-bedrock-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") 'aws-bedrock-send-input)
    map))

;;;###autoload
(defun aws-bedrock ()
  "Open the AWS Bedrock chat interface."
  (interactive)
  (aws--pop-to-buffer (aws--buffer-name "bedrock-chat"))
  (aws-bedrock-mode))

(define-derived-mode aws-bedrock-mode markdown-mode "aws-bedrock"
  "AWS Bedrock Chat mode with markdown support"
  (setq major-mode 'aws-bedrock-mode)
  (use-local-map aws-bedrock-mode-map)
  (setq aws-bedrock--conversation-history nil)
  ;; Disable markdown-mode's electric pairing that might interfere
  (setq-local markdown-enable-math nil)
  (setq-local markdown-enable-wiki-links nil)
  ;; Make buffer writable (not the text properties, the buffer itself)
  (setq buffer-read-only nil)
  (aws-bedrock--setup-buffer)
  ;; Add hooks to protect read-only area and restore protective space
  (add-hook 'before-change-functions #'aws-bedrock--protect-input-area nil t)
  (add-hook 'after-change-functions #'aws-bedrock--restore-writable-space nil t))

(provide 'aws-bedrock)
;;; aws-bedrock.el ends here
