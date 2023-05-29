;;; gptel-extensions.el --- Extra functions for gptel

;; Author: Henrique Goncalves <kamus@hadenes.io>
;; URL: https://github.com/kamushadenes
;; Version: 0.0.1

;;; Commentary:
;;
;; Extend gptel with some extra functions
;;
;;; Code:

(require 'gptel)

;;;###autoload
(defun gptel-ext-send-whole-buffer ()
  "Send the whole buffer to ChatGPT."
  (interactive)
  (mark-whole-buffer)
  (goto-char (point-max))
  (gptel-send))

;;;###autoload
(defun gptel-ext-ask-document ()
  "Loads the current buffer into a session so you can ask questions about it."
  (interactive)
  (let ((nbuf (concat "Ask: " (buffer-name (current-buffer)))))
    (gptel
     nbuf
     :initial (concat
               "Your task is to answer questions about the following document. If you don't know the answer, reply with \"I don't know\"\n\n###### DOCUMENT START ######\n\n"
               (buffer-substring-no-properties (point-min) (point-max))
               "\n\n###### DOCUMENT END ######\n\n### Question: "))
    (pop-to-buffer nbuf)))


;; extracted from the wiki
;;
;;;###autoload
(defun gptel-ext-rewrite-and-replace (bounds &optional directive)
  "Rewrite the region or sentence at point and replace it with the response."
  (interactive
   (list
    (cond
     ((use-region-p) (cons (region-beginning) (region-end)))
     ((derived-mode-p 'text-mode)
      (list (bounds-of-thing-at-point 'sentence)))
     (t (cons (line-beginning-position) (line-end-position))))
    (and current-prefix-arg
         (read-string "ChatGPT Directive: "
                      "You are a prose editor. Rewrite my prompt more professionally."))))
  (gptel-request
   (buffer-substring-no-properties (car bounds) (cdr bounds)) ;the prompt
   :system (or directive "You are a prose editor. Rewrite my prompt more professionally.")
   :buffer (current-buffer)
   :context (cons (set-marker (make-marker) (car bounds))
                  (set-marker (make-marker) (cdr bounds)))
   :callback
   (lambda (response info)
     (if (not response)
         (message "ChatGPT response failed with: %s" (plist-get info :status))
       (let* ((bounds (plist-get info :context))
              (beg (car bounds))
              (end (cdr bounds))
              (buf (plist-get info :buffer)))
         (with-current-buffer buf
           (save-excursion
             (goto-char beg)
             (kill-region beg end)
             (insert response)
             (set-marker beg nil)
             (set-marker end nil)
             (message "Rewrote line. Original line saved to kill-ring."))))))))

;;;###autoload
(defun gptel-ext-refactor (bounds)
  "Refactor the region or sentence at point."
  (gptel-ext-rewrite-and-replace bounds "You are a programmer. Refactor my code to improve readability."))

(provide 'gptel-extensions)

;;; gptel-extensions.el ends here
