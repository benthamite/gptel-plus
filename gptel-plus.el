;;; gptel-plus.el --- Enhancements for gptel -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/gptel-plus
;; Version: 0.1
;; Package-Requires: ((el-patch "3.1") (gptel "0.7.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Enhancements for gptel.

;;; Code:

(require 'gptel)
(require 'gptel-context)
(require 'el-patch)

;;;; User options

(defgroup gptel-plus ()
  "Enhancements for `gptel'."
  :group 'gptel)

(defcustom gptel-plus-tokens-per-word 1.5
  "The approximate number of tokens per word.
Used to estimate input costs, based on the number of words in the prompt."
  :type 'number
  :group 'gptel-plus)

(defcustom gptel-plus-tokens-in-output 100
  "The average number of tokens in the response output.
Used to estimate output costs."
  :type 'number
  :group 'gptel-plus)

(defcustom gptel-plus-cost-warning-threshold 0.15
  "The cost threshold above which to display a warning before sending a prompt.
To disable warnings, set this value to nil."
  :type 'number
  :group 'gptel-plus)

(defvar gptel-plus--context-cost nil
  "Cached cost calculation for context files.")


;;;;; Cost estimation

;; TODO: estimate cost added via `gptel-context--add-region'
(defun gptel-plus-get-total-cost ()
  "Get the rough cost of prompting the current model.
This is used to display the relevant information in the `gptel' headerline.

The input cost is approximated based on the number of words in the buffer or
selection. The function uses a default 1.4 token/word conversion factor, but the
actual cost may deviate from this estimate. (To change this default, customize
`gptel-plus-tokens-per-word'.) For the output cost, we simply assume a
response of 100 tokens, which appears to be the average LLM response length. (To
change this default, customize `gptel-plus-tokens-in-output'.)

Note that, currently, images are not included in the cost calculation."
  (when-let ((input-cost (gptel-plus-get-input-cost))
             (output-cost (gptel-plus-get-output-cost)))
    (gptel-plus-normalize-cost (+ input-cost output-cost))))

(defun gptel-plus-get-input-cost ()
  "Return cost for the input."
  (when-let* ((buffer-cost (gptel-plus-get-buffer-cost)))
    (+ buffer-cost (or gptel-plus--context-cost 0))))

(defun gptel-plus-get-output-cost ()
  "Return cost for the output."
  (when-let* ((cost-per-1m-output-tokens (get gptel-model :output-cost))
	      (tokens-in-output gptel-plus-tokens-in-output))
    (* cost-per-1m-output-tokens tokens-in-output)))

(defun gptel-plus-normalize-cost (cost)
  "Normalize COST to a dollar amount."
  (/ cost 1000000.0))

(defun gptel-plus-get-context-cost ()
  "Return cost for the current context files."
  (gptel-plus-get-cost-of-input-type 'context))

(defun gptel-plus-get-buffer-cost ()
  "Return cost for the current buffer or region."
  (gptel-plus-get-cost-of-input-type 'buffer))

(defun gptel-plus-get-cost-of-input-type (type)
  "Get the cost of the current buffer or the context files.
TYPE is either `buffer' or `context'."
  (when-let* ((cost-per-1m-input-tokens (get gptel-model :input-cost))
              (tokens-per-word gptel-plus-tokens-per-word)
              (words-context (pcase type
			       ('buffer (gptel-plus-count-words-in-buffer))
			       ('context (gptel-plus-count-words-in-context)))))
    (* cost-per-1m-input-tokens tokens-per-word words-context)))

(defun gptel-plus-update-context-cost (&rest _)
  "Update the context cost when the context is modified."
  (setq gptel-plus--context-cost (gptel-plus-get-context-cost)))

(advice-add 'gptel-context-add-file :after #'gptel-plus-update-context-cost)
(advice-add 'gptel-context-remove :after #'gptel-plus-update-context-cost)

(defun gptel-plus--update-cost-on-model-change (sym _ &optional _)
  "Update context cost when SYM is `gptel-model' or `gptel-backend'."
  (when (memq sym '(gptel-model gptel-backend))
    (gptel-plus-update-context-cost)))

(advice-add 'gptel--set-with-scope :after #'gptel-plus--update-cost-on-model-change)

;; TODO: handle restricted
;; (https://github.com/karthink/gptel#limit-conversation-context-to-an-org-heading)
;; and branching
;; (https://github.com/karthink/gptel#use-branching-context-in-org-mode-tree-of-conversations)
;; conversations
(defun gptel-plus-count-words-in-buffer ()
  "Count the number of words in the current buffer or region."
  (if (region-active-p)
      (count-words (region-beginning) (region-end))
    (count-words (point-min) (point))))

(defun gptel-plus-count-words-in-context ()
  "Iterate over the files and buffers in context and add up the word count in each.
Binaries are skipped. Also works with buffers in context."
  (let ((revert-without-query t)
	(initial-buffers (buffer-list)))
    (prog1
	(cl-reduce (lambda (accum item)
		     (let ((file-or-buffer (car item)))
		       (cond
			;; If it's a buffer
			((bufferp file-or-buffer)
			 (+ accum
			    (with-current-buffer file-or-buffer
			      (count-words (point-min) (point-max)))))
			;; If it's a file
			((stringp file-or-buffer)
			 (if (gptel--file-binary-p file-or-buffer)
			     accum
			   (+ accum
			      (with-temp-buffer
				(insert-file-contents file-or-buffer)
				(count-words (point-min) (point-max))))))
			;; Otherwise (shouldn't happen)
			(t accum))))
		   gptel-context--alist
		   :initial-value 0)
      ;; Clean up any temp buffers we created
      (dolist (buf (buffer-list))
	(unless (member buf initial-buffers)
	  (kill-buffer buf))))))

(defun gptel-plus-confirm-when-costs-high (&optional _)
  "Prompt user for confirmation if the cost of current prompt exceeds threshold.
The threshold is set via `gptel-plus-cost-warning-threshold'."
  (let ((cost (gptel-plus-get-total-cost)))
    (when-let ((threshold gptel-plus-cost-warning-threshold))
      (when (> cost threshold)
	(unless (y-or-n-p (format "The cost of this prompt is $%.2f. Continue? " cost))
	  (user-error "Prompt cancelled"))))))

(advice-add 'gptel-send :before #'gptel-plus-confirm-when-costs-high)

;;;;;; Display costs
;; This is just the original `gptel-mode' definition with a modification to add
;; an additional cost field in the header line.
(with-eval-after-load 'gptel
  (el-patch-define-minor-mode gptel-mode
    "Minor mode for interacting with LLMs."
    :lighter " GPT"
    :keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-c RET") #'gptel-send)
      map)
    (if gptel-mode
	(progn
	  (unless (derived-mode-p 'org-mode 'markdown-mode 'text-mode)
	    (gptel-mode -1)
	    (user-error (format "`gptel-mode' is not supported in `%s'." major-mode)))
	  (add-hook 'before-save-hook #'gptel--save-state nil t)
	  (when (derived-mode-p 'org-mode)
            ;; Work around bug in `org-fontify-extend-region'.
            (add-hook 'gptel-post-response-functions #'gptel--font-lock-update nil t))
	  (gptel--restore-state)
	  (if gptel-use-header-line
	      (setq gptel--old-header-line header-line-format
		    header-line-format
		    (list '(:eval (concat (propertize " " 'display '(space :align-to 0))
					  (format "%s" (gptel-backend-name gptel-backend))))
			  (propertize " Ready" 'face 'success)
			  '(:eval
			    (let* ((model (gptel--model-name gptel-model))
				   (system
				    (propertize
				     (buttonize
				      (format "[Prompt: %s]"
					      (or (car-safe (rassoc gptel--system-message gptel-directives))
						  (gptel--describe-directive gptel--system-message 15)))
				      (lambda (&rest _) (gptel-system-prompt)))
				     'mouse-face 'highlight
				     'help-echo "System message for session"))
				   (el-patch-add
				     (cost (let* ((cost (gptel-plus-get-total-cost))
						  (cost-msg (if cost
								(format "[Cost: $%.2f]" cost)
							      "[Cost: N/A]")))
					     (propertize
					      (buttonize cost-msg
							 (lambda (&rest _) (gptel-menu)))
					      'mouse-face 'highlight
					      'help-echo (if cost
							     "Cost of the current prompt"
							   "There is no cost information available for this model")))))
				   (context
				    (and gptel-context--alist
					 (cl-loop for entry in gptel-context--alist
						  if (bufferp (car entry)) count it into bufs
						  else count (stringp (car entry)) into files
						  finally return
						  (propertize
						   (buttonize
						    (concat "[Context: "
							    (and (> bufs 0) (format "%d buf" bufs))
							    (and (> bufs 1) "s")
							    (and (> bufs 0) (> files 0) ", ")
							    (and (> files 0) (format "%d file" files))
							    (and (> files 1) "s")
							    "]")
						    (lambda (&rest _)
						      (require 'gptel-context)
						      (gptel-context--buffer-setup)))
						   'mouse-face 'highlight
						   'help-echo "Active gptel context"))))
				   (toggle-track-media
				    (lambda (&rest _)
				      (setq-local gptel-track-media
						  (not gptel-track-media))
				      (if gptel-track-media
					  (message
					   (concat
					    "Sending media from included links.  To include media, create "
					    "a \"standalone\" link in a paragraph by itself, separated from surrounding text."))
					(message "Ignoring image links.  Only link text will be sent."))
				      (run-at-time 0 nil #'force-mode-line-update)))
				   (track-media
				    (and (gptel--model-capable-p 'media)
					 (if gptel-track-media
					     (propertize
					      (buttonize "[Sending media]" toggle-track-media)
					      'mouse-face 'highlight
					      'help-echo
					      "Sending media from standalone links/urls when supported.\nClick to toggle")
					   (propertize
					    (buttonize "[Ignoring media]" toggle-track-media)
					    'mouse-face 'highlight
					    'help-echo
					    "Ignoring images from standalone links/urls.\nClick to toggle"))))
				   (toggle-tools (lambda (&rest _) (interactive)
						   (run-at-time 0 nil
								(lambda () (call-interactively #'gptel-tools)))))
				   (tools (when (and gptel-use-tools gptel-tools)
					    (propertize
					     (buttonize (pcase (length gptel-tools)
							  (0 "[No tools]") (1 "[1 tool]")
							  (len (format "[%d tools]" len)))
							toggle-tools)
					     'mouse-face 'highlight
					     'help-echo "Select tools"))))
			      (concat
			       (propertize
				" " 'display
				`(space :align-to (- right
						     ,(+ 5 (length model) (length system)
							 (length track-media) (length context)
							 (el-patch-add (length cost))
							 (length tools)))))
			       tools (and track-media " ") track-media (and context " ") context " "
			       (el-patch-add cost " ")
			       system " "
			       (propertize
				(buttonize (concat "[" model "]")
					   (lambda (&rest _) (gptel-menu)))
				'mouse-face 'highlight
				'help-echo "Model in use"))))))
	    (setq mode-line-process
		  '(:eval (concat " "
				  (buttonize (gptel--model-name gptel-model)
					     (lambda (&rest _) (gptel-menu))))))))
      (remove-hook 'before-save-hook #'gptel--save-state t)
      (if gptel-use-header-line
	  (setq header-line-format gptel--old-header-line
		gptel--old-header-line nil)
	(setq mode-line-process nil)))))

;;;;; Automatic mode activation

(defconst gptel-plus-local-variables
  '(gptel-mode gptel-model gptel--backend-name gptel--bounds)
  "A list of relevant `gptel' file-local variables.")

(defconst gptel-plus-org-properties
  '("GPTEL_SYSTEM" "GPTEL_BACKEND" "GPTEL_MODEL"
    "GPTEL_TEMPERATURE" "GPTEL_MAX_TOKENS"
    "GPTEL_NUM_MESSAGES_TO_SEND")
  "A list of relevant `gptel' Org properties.")

(defun gptel-plus-enable-gptel-in-org ()
  "Enable `gptel-mode' in `org-mode' files with `gptel' data."
  (when (gptel-plus-file-has-gptel-org-property-p)
    (gptel-plus-enable-gptel-common)))

(defun gptel-plus-enable-gptel-in-markdown ()
  "Enable `gptel-mode' in `markdown-mode' files with `gptel' data."
  (when (gptel-plus-file-has-gptel-local-variable-p)
    (gptel-plus-enable-gptel-common)))

(declare-function breadcrumb-mode "breadcrumb")
(defun gptel-plus-enable-gptel-common ()
  "Enable `gptel-mode' and in any buffer with `gptel' data."
  (let ((buffer-modified-p (buffer-modified-p)))
    (gptel-mode)
    ;; `breadcrumb-mode' interferes with the `gptel' header line
    (when (bound-and-true-p breadcrumb-mode)
      (breadcrumb-mode -1))
    ;; prevent the buffer from becoming modified merely because `gptel-mode'
    ;; is enabled
    (unless buffer-modified-p
      (save-buffer))))

(defun gptel-plus-file-has-gptel-local-variable-p ()
  "Return t iff the current buffer has a `gptel' local variable."
  (cl-some (lambda (var)
	     (local-variable-p var))
	   gptel-plus-local-variables))

(autoload 'org-entry-get "org")
(defun gptel-plus-file-has-gptel-org-property-p ()
  "Return t iff the current buffer has a `gptel' Org property."
  (cl-some (lambda (prop)
	     (org-entry-get (point-min) prop))
	   gptel-plus-org-properties))

;;;;; Context persistence

(defvar-local gptel-plus-context nil
  "The context for the current buffer.")

;;;;;; Save

(autoload 'org-set-property "org")
(defun gptel-plus-save-file-context ()
  "Save the current `gptel' file context in file visited by the current buffer.
In Org files, saves as a file property. In Markdown, as a file-local variable."
  (interactive)
  (if (derived-mode-p 'org-mode 'markdown-mode)
      (when (or (not (gptel-plus-get-saved-context))
		(yes-or-no-p "Overwrite existing file context? "))
	(pcase major-mode
	  ('org-mode (gptel-plus-save-file-context-in-org))
	  ('markdown-mode (gptel-plus-save-file-context-in-markdown)))
	(message "Saved `gptel' context: %s" (prin1-to-string gptel-context--alist)))
    (user-error "Not in and Org or Markdown buffer")))

(defun gptel-plus-save-file-context-in-org ()
  "Save the current `gptel' file context in file visited by the current Org buffer."
  (save-excursion
    (goto-char (point-min))
    (org-set-property "GPTEL_CONTEXT" (prin1-to-string gptel-context--alist))))

(defun gptel-plus-save-file-context-in-markdown ()
  "Save the current `gptel' file context in file visited by the current MD buffer."
  (gptel-plus-remove-local-variables-section)
  (let ((context (format "%S" gptel-context--alist)))
    (add-file-local-variable 'gptel-plus-context context)))

(defun gptel-plus-remove-local-variables-section ()
  "Remove the existing Local Variables section from the current buffer."
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward "^<!-- Local Variables: -->" nil t)
      (let ((start (point)))
        (when (re-search-forward "^<!-- End: -->" nil t)
          (delete-region start (point))
          (delete-blank-lines))))))

;;;;;; Get saved

(defun gptel-plus-get-saved-context ()
  "Get the saved `gptel' context from the file visited by the current buffer."
  (pcase major-mode
    ('org-mode
     (when-let* ((gptel-context-prop (org-entry-get (point-min) "GPTEL_CONTEXT")))
       (read gptel-context-prop)))
    ('markdown-mode gptel-plus-context)
    (_ (user-error "Not in and Org or Markdown buffer"))))

;;;;;; Restore

(defun gptel-plus-restore-file-context ()
  "Restore the saved file context from the file visited by the current buffer."
  (interactive)
  (if-let ((context (gptel-plus-get-saved-context)))
      (when (or (not gptel-context--alist)
		(y-or-n-p "Overwrite current `gptel' context? "))
	(gptel-context-remove-all)
	(mapc (lambda (monolist)
		(gptel-context-add-file (car monolist)))
	      context))
    (message "No saved `gptel' context found.")))

;;;;; Context management

(defun gptel-plus-list-context-files ()
  "List all files in the current `gptel' context sorted by size.
Each file is shown along with its size."
  (interactive)
  (if gptel-context--alist
      (with-current-buffer (get-buffer-create "*gptel context files*")
        (gptel-context-files-mode)
        (gptel-plus-list-context-files-internal)
        (pop-to-buffer (current-buffer)))
    (message "No files in context.")))

(defun gptel-plus-list-context-files-internal ()
  "Populate the current buffer with the gptel context files in a flaggable format.
Lists key bindings dynamically based on the current mode's keymap."
  (let* ((key-bindings (gptel-context-files--describe-keybindings (current-local-map)))
         (files (cl-remove-if-not #'stringp (mapcar #'car gptel-context--alist)))
         (file-sizes (mapcar (lambda (f)
                               (cons f (file-attribute-size (file-attributes f))))
                             files))
         (sorted-files (sort file-sizes (lambda (a b) (> (cdr a) (cdr b)))))
         (home-dir (expand-file-name "~/")))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Context files (sorted by size):\n")
      (insert (format "\n%s\n\n" key-bindings))
      (dolist (entry sorted-files)
        (let* ((file (car entry))
               (display-file (if (string-prefix-p home-dir file)
                                 (concat "~/" (substring file (length home-dir)))
                               file))
               (size (cdr entry))
               (start (point)))
          (insert (format "[ ]\t%.2f KB\t%s\n" (/ size 1024.0) display-file))
          (put-text-property start (+ start 3) 'gptel-context-file file)
          (put-text-property start (+ start 3) 'gptel-flag nil))))
    (goto-char (point-min))))

(define-derived-mode gptel-context-files-mode special-mode "GPT Context Files"
  "Major mode for flagging gptel context files for removal."
  (setq-local truncate-lines t)
  (hl-line-mode)
  (use-local-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "x") #'gptel-plus-toggle-mark)
     (define-key map (kbd "D") #'gptel-plus-remove-flagged-context-files)
     (define-key map (kbd "g") #'gptel-plus-refresh-context-files-buffer)
     (define-key map (kbd "q") #'kill-this-buffer)
     map))
  (read-only-mode 1))

(defun gptel-context-files--describe-keybindings (keymap)
  "Return a string description of KEYMAP's bindings in the format: key = command."
  (let ((bindings '()))
    (map-keymap
     (lambda (event binding)
       (when (and (not (keymapp binding))
                  (commandp binding))
         (let ((key-str (key-description (vector event)))
	       (cmd-str (if (symbolp binding)
                            (symbol-name binding)
                          (prin1-to-string binding))))
           (push (format "%s = %s" key-str cmd-str) bindings))))
     keymap)
    (mapconcat 'identity (sort bindings 'string<) "\n")))

(defun gptel-plus-toggle-mark ()
  "Toggle the mark on the current line’s file entry and move to the next entry."
  (interactive)
  (let ((line-start (line-beginning-position)))
    (when-let ((file (get-text-property line-start 'gptel-context-file)))
      (let* ((current-flag (get-text-property line-start 'gptel-flag))
             (new-flag (not current-flag))
             (new-marker (if new-flag "[X]" "[ ]")))
        (let ((inhibit-read-only t))
          (delete-region line-start (+ line-start 3))
          (goto-char line-start)
          (insert new-marker)
          (put-text-property line-start (+ line-start 3) 'gptel-context-file file)
          (put-text-property line-start (+ line-start 3) 'gptel-flag new-flag)))
      (forward-line 1))))

(defun gptel-plus-remove-flagged-context-files ()
  "Remove from the gptel context all files that have been flagged in this buffer.
This command scans the buffer for file entries where the marker property
`gptel-flag' is non-nil, removes those files from `gptel-context--alist’,
updates the cost, and then refreshes the buffer."
  (interactive)
  (let (files-to-remove)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (and (get-text-property (line-beginning-position) 'gptel-context-file)
                   (get-text-property (line-beginning-position) 'gptel-flag))
          (push (get-text-property (line-beginning-position) 'gptel-context-file)
                files-to-remove))
        (forward-line 1)))
    (if files-to-remove
        (progn
          ;; Remove each flagged file from the context:
          (dolist (file files-to-remove)
            (setq gptel-context--alist (assq-delete-all file gptel-context--alist)))
          (gptel-plus-update-context-cost)
          (message "Removed flagged files from context: %s"
                   (mapconcat 'identity files-to-remove ", "))
          (gptel-plus-refresh-context-files-buffer))
      (message "No files flagged for removal."))))

(defun gptel-plus-refresh-context-files-buffer ()
  "Refresh the buffer showing the gptel context-files list."
  (interactive)
  (when-let ((buf (get-buffer "*gptel context files*")))
    (with-current-buffer buf
      (gptel-plus-list-context-files-internal)
      (message "Context file listing refreshed."))))

(provide 'gptel-plus)
;;; gptel-plus.el ends here

