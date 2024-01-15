;;; gptel-extensions.el --- Extra functions for gptel

;; Author: Henrique Goncalves <kamus@hadenes.io>
;; URL: https://github.com/kamushadenes
;; Version: 0.0.2
;; Package-Requires: ((gptel "0.3.6"))
;; Keywords: gpt, ai, openai, gptel

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Extend gptel with some extra functions

;;; Code:

(require 'gptel)

(defgroup gptel-ext nil
  "Extensions for gptel."
  :group 'gptel-ext)

(defvar gptel-extensions-ask-document-prefix "Your task is to answer questions about the following document. If you don't know the answer, reply with \"I don't know\"\n\n###### DOCUMENT START ######\n\n"
  "Prefix to use when asking questions about a document.")

(defvar gptel-extensions-ask-document-suffix "\n\n###### DOCUMENT END ######\n\n### Question: "
  "Suffix to use when asking questions about a document.")

(defvar gptel-extensions-refactor-directive "You are a programmer. Refactor my code to improve readability. Reply only with the code."
  "Directive to use when refactoring code.")

;;;###autoload
(defun gptel-extensions-send-whole-buffer ()
  "Send the whole buffer to ChatGPT."
  (interactive)
  (mark-whole-buffer)
  (goto-char (point-max))
  (gptel-send))

;;;###autoload
(defun gptel-extensions-ask-document ()
  "Loads the current buffer into a session so you can ask questions about it."
  (interactive)
  (let ((nbuf (concat "Ask: " (buffer-name (current-buffer)))))
    (gptel
     nbuf
     :initial (concat
               gptel-extensions-ask-document-prefix
               (buffer-substring-no-properties (point-min) (point-max))
               gptel-extensions-ask-document-suffix))
    (pop-to-buffer nbuf)))

;; extracted from the wiki
;;
;;;###autoload
(defun gptel-extensions-rewrite-and-replace (bounds &optional directive)
  "Rewrite the region or sentence at point and replace it with the response.

BOUNDS is a cons cell where the car is the beginning and the cdr is the end of the region to be rewritten."
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
(defun gptel-extensions-refactor (bounds)
  "Refactor the region or sentence at point.

BOUNDS is a cons cell where the car is the beginning and the cdr is the end of the region to be refactored."
  (interactive
   (list
    (cond
     ((use-region-p) (cons (region-beginning) (region-end)))
     ((derived-mode-p 'text-mode)
      (list (bounds-of-thing-at-point 'sentence)))
     (t (cons (line-beginning-position) (line-end-position))))))
  (message "Refactoring...")
  (gptel-extensions-rewrite-and-replace bounds gptel-extensions-refactor-directive))

(provide 'gptel-extensions)

;;; gptel-extensions.el ends here
