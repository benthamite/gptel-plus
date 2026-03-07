;;; gptel-plus.el --- Enhancements for gptel -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 Pablo Stafforini

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/gptel-plus
;; Version: 0.3
;; Package-Requires: ((emacs "29.1") (gptel "0.7.1"))

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
;;
;; Cost calculation:
;;
;; gptel-plus provides two complementary mechanisms for computing
;; the cost of LLM requests:
;;
;; 1. Automatic display for `gptel-send' (interactive chat):
;;    Hooks into `gptel-post-response-functions' to parse the log
;;    buffer and display cost after each response.  Controlled by
;;    `gptel-plus-calculate-cost'.
;;
;; 2. Public API for `gptel-request' (programmatic use):
;;    Advises `gptel--parse-response' to capture the raw token
;;    usage from API responses into the INFO plist (under
;;    `:token-usage').  External packages can then call
;;    `gptel-plus-compute-cost' from their callbacks:
;;
;;      (gptel-request prompt
;;        :callback (lambda (response info)
;;                    (let ((cost (gptel-plus-compute-cost info)))
;;                      (message "Cost: $%.4f" cost))))
;;
;;    `gptel-plus-compute-cost' handles Anthropic, OpenAI, and
;;    Gemini usage formats, including prompt caching.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'gptel-context)
(require 'json)

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
  :type '(choice number (const :tag "Disabled" nil))
  :group 'gptel-plus)

(defcustom gptel-plus-calculate-cost t
  "Whether to calculate the cost of `gptel' requests."
  :type 'boolean
  :group 'gptel-plus)

(defvar gptel-plus--context-cost nil
  "Cached cost calculation for context files.")

(defvar gptel-plus--logging-requests-count 0
  "Counter for active `gptel' requests that require logging.")

(defvar gptel-plus--original-log-level nil
  "Original value of `gptel-log-level' before being temporarily changed.")

(defvar gptel-plus--original-header-line-info nil
  "Original value of `gptel--header-line-info' before gptel-plus replaced it.")

(defvar-local gptel-plus--log-start-position nil
  "Position in `*gptel-log*' when the current request was initiated.
Used to scope log parsing to the correct request's data.")

;;;;; ex ante cost estimation

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
  (when gptel-plus-calculate-cost
    (when-let* ((input-cost (gptel-plus-get-input-cost))
                (output-cost (gptel-plus-get-output-cost)))
      (gptel-plus-normalize-cost (+ input-cost output-cost)))))

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
  (cl-loop for (file-or-buffer) in gptel-context
           sum (cond
                ((and (bufferp file-or-buffer)
                      (buffer-live-p file-or-buffer))
                 (with-current-buffer file-or-buffer
                   (count-words (point-min) (point-max))))
                ((and (stringp file-or-buffer)
                      (file-readable-p file-or-buffer)
                      (not (gptel--file-binary-p file-or-buffer)))
                 (with-temp-buffer
                   (insert-file-contents file-or-buffer)
                   (count-words (point-min) (point-max))))
                (t 0))))

(defun gptel-plus-confirm-when-costs-high (&optional _)
  "Prompt user for confirmation if the cost of current prompt exceeds threshold.
The threshold is set via `gptel-plus-cost-warning-threshold'."
  (let ((cost (gptel-plus-get-total-cost)))
    (when-let* ((threshold gptel-plus-cost-warning-threshold))
      (when (and cost (> cost threshold))
	(unless (y-or-n-p (format "The cost of this prompt is $%.2f. Continue? " cost))
	  (when (y-or-n-p "Clear context? ")
	    (gptel-context-remove-all))
	  (user-error "Prompt cancelled"))))))

(advice-add 'gptel-send :before #'gptel-plus-confirm-when-costs-high)

;;;;; ex post cost estimation

(declare-function gptel-openai-p "gptel-openai")
(declare-function gptel-anthropic-p "gptel-anthropic")
(declare-function gptel-gemini-p "gptel-gemini")

;;;;;; Usage capture

(defun gptel-plus--capture-usage (orig-fn backend response proc-info)
  "Around advice for `gptel--parse-response' to capture token usage.
Extracts the usage plist from the raw API response and stores it
in PROC-INFO under `:token-usage' before the response is stripped
to text content.  This makes token data available to `gptel-request'
callbacks via the INFO plist, enabling cost calculation for any
gptel consumer (not just `gptel-send')."
  (when-let* ((usage (or (plist-get response :usage)
                         (plist-get response :usageMetadata))))
    (plist-put proc-info :token-usage usage))
  (funcall orig-fn backend response proc-info))

(advice-add 'gptel--parse-response :around #'gptel-plus--capture-usage)

(defun gptel-plus-compute-cost (info &optional model)
  "Compute the dollar cost of a gptel request from its INFO plist.
INFO is the plist passed to a `gptel-request' callback.  MODEL is the
model symbol to look up pricing for; it defaults to `gptel-model'.

Returns the cost as a float, or nil if usage data or pricing is
unavailable.  Handles Anthropic, OpenAI, and Gemini usage formats,
including prompt caching."
  (let ((model (or model gptel-model)))
    (when-let* ((usage (plist-get info :token-usage))
                (input-rate (get model :input-cost))
                (output-rate (get model :output-cost)))
      (let (input-tokens output-tokens cache-adj)
        (cond
         ;; Anthropic: :input_tokens, :output_tokens
         ((plist-member usage :input_tokens)
          (setq input-tokens (plist-get usage :input_tokens)
                output-tokens (or (plist-get usage :output_tokens) 0)
                cache-adj (+ (* (or (plist-get usage :cache_creation_input_tokens) 0)
                                input-rate 1.25)
                             (* (or (plist-get usage :cache_read_input_tokens) 0)
                                input-rate 0.1))))
         ;; OpenAI: :prompt_tokens, :completion_tokens
         ((plist-member usage :prompt_tokens)
          (setq input-tokens (plist-get usage :prompt_tokens)
                output-tokens (or (plist-get usage :completion_tokens) 0)
                cache-adj (* (or (when-let* ((d (plist-get usage :prompt_tokens_details)))
                                   (plist-get d :cached_tokens))
                                 0)
                             input-rate -0.5)))
         ;; Gemini: :promptTokenCount, :candidatesTokenCount
         ((plist-member usage :promptTokenCount)
          (setq input-tokens (plist-get usage :promptTokenCount)
                output-tokens (or (plist-get usage :candidatesTokenCount) 0)
                cache-adj 0))
         (t (setq input-tokens 0 output-tokens 0 cache-adj 0)))
        (when (> (+ input-tokens output-tokens) 0)
          (/ (+ (* input-tokens input-rate)
                (* output-tokens output-rate)
                (or cache-adj 0))
             1000000.0))))))

(defun gptel-plus--backend-type ()
  "Return a symbol for the type of the current `gptel' backend.
Returns `anthropic', `openai', `gemini', or nil for unsupported backends."
  (cond
   ((and (fboundp 'gptel-anthropic-p)
         (funcall 'gptel-anthropic-p gptel-backend))
    'anthropic)
   ((and (fboundp 'gptel-gemini-p)
         (funcall 'gptel-gemini-p gptel-backend))
    'gemini)
   ((and (fboundp 'gptel-openai-p)
         (funcall 'gptel-openai-p gptel-backend))
    'openai)))

(defun gptel-plus--add-stream-options (orig-fn backend prompts)
  "Around advice for `gptel--request-data' to add OpenAI stream_options.
Injects `stream_options' so that streaming responses include token usage."
  (let ((result (funcall orig-fn backend prompts)))
    (when (and gptel-plus-calculate-cost
               gptel-stream
               (fboundp 'gptel-openai-p)
               (funcall 'gptel-openai-p backend)
               (not (plist-member result :stream_options)))
      (plist-put result :stream_options '(:include_usage t)))
    result))

(advice-add 'gptel--request-data :around #'gptel-plus--add-stream-options)

(defun gptel-plus--read-log-event-json ()
  "Read the JSON data line following point in the log buffer.
Return the parsed JSON object, or nil if no data line is found."
  (when (re-search-forward "^data: \\(.*\\)" nil t)
    (json-read-from-string (match-string 1))))

(defun gptel-plus--extract-request-tokens (backend-type)
  "Extract token counts and model from the log buffer.
BACKEND-TYPE is `anthropic', `openai', or `gemini'.
Return a plist (:input-tokens N :output-tokens N :model SYM), or nil."
  (pcase backend-type
    ('anthropic (gptel-plus--extract-tokens-anthropic))
    ('openai    (gptel-plus--extract-tokens-openai))
    ('gemini    (gptel-plus--extract-tokens-gemini))))

(defun gptel-plus--extract-tokens-anthropic ()
  "Extract token counts and model from an Anthropic response in the log buffer.
Handles both streaming (SSE events) and non-streaming (single JSON) formats.
Returns cache token counts when prompt caching is in use."
  (goto-char (point-max))
  (cond
   ((re-search-backward "^event: message_delta" nil t)
    (gptel-plus--extract-tokens-anthropic-streaming))
   ;; Only try non-streaming when no SSE events are present at all,
   ;; to avoid matching partial streaming data.
   ((progn (goto-char (point-min))
           (not (re-search-forward "^event: " nil t)))
    (gptel-plus--extract-tokens-anthropic-non-streaming))))

(defun gptel-plus--extract-tokens-anthropic-streaming ()
  "Extract tokens from an Anthropic streaming response in the log buffer."
  (let ((end-of-request-pos (point)))
    (when-let* ((json (gptel-plus--read-log-event-json))
                (usage (cdr (assoc 'usage json)))
                (output-tokens (cdr (assoc 'output_tokens usage))))
      (goto-char end-of-request-pos)
      (when (re-search-backward "^event: message_start" nil t)
        (when-let* ((start-json (gptel-plus--read-log-event-json))
                    (message-data (cdr (assoc 'message start-json)))
                    (start-usage (cdr (assoc 'usage message-data)))
                    (input-tokens (cdr (assoc 'input_tokens start-usage)))
                    (model-id (cdr (assoc 'model message-data)))
                    (model-sym (intern-soft model-id)))
          (list :input-tokens input-tokens
                :output-tokens output-tokens
                :model model-sym
                :cache-creation-tokens
                (or (cdr (assoc 'cache_creation_input_tokens start-usage)) 0)
                :cache-read-tokens
                (or (cdr (assoc 'cache_read_input_tokens start-usage)) 0)))))))

(defun gptel-plus--extract-tokens-anthropic-non-streaming ()
  "Extract tokens from an Anthropic non-streaming response in the log buffer."
  (let (input-tokens output-tokens cache-creation cache-read model-sym)
    (goto-char (point-max))
    (when (re-search-backward
           "\"input_tokens\"[ \t\n]*:[ \t\n]*\\([0-9]+\\)" nil t)
      (setq input-tokens (string-to-number (match-string 1))))
    (goto-char (point-max))
    (when (re-search-backward
           "\"output_tokens\"[ \t\n]*:[ \t\n]*\\([0-9]+\\)" nil t)
      (setq output-tokens (string-to-number (match-string 1))))
    (goto-char (point-max))
    (when (re-search-backward
           "\"cache_creation_input_tokens\"[ \t\n]*:[ \t\n]*\\([0-9]+\\)" nil t)
      (setq cache-creation (string-to-number (match-string 1))))
    (goto-char (point-max))
    (when (re-search-backward
           "\"cache_read_input_tokens\"[ \t\n]*:[ \t\n]*\\([0-9]+\\)" nil t)
      (setq cache-read (string-to-number (match-string 1))))
    (goto-char (point-max))
    (when (re-search-backward
           "\"model\"[ \t\n]*:[ \t\n]*\"\\([^\"]+\\)\"" nil t)
      (setq model-sym (intern-soft (match-string 1))))
    (when (and input-tokens output-tokens)
      (list :input-tokens input-tokens
            :output-tokens output-tokens
            :model model-sym
            :cache-creation-tokens (or cache-creation 0)
            :cache-read-tokens (or cache-read 0)))))

(defun gptel-plus--extract-tokens-openai ()
  "Extract token counts and model from an OpenAI response in the log buffer.
Works for both streaming (with stream_options) and non-streaming responses.
Extracts cached token counts when prompt caching is in use."
  (goto-char (point-max))
  (let (input-tokens output-tokens cached-tokens model-sym)
    (when (re-search-backward
           "\"prompt_tokens\"[ \t\n]*:[ \t\n]*\\([0-9]+\\)" nil t)
      (setq input-tokens (string-to-number (match-string 1))))
    (goto-char (point-max))
    (when (re-search-backward
           "\"completion_tokens\"[ \t\n]*:[ \t\n]*\\([0-9]+\\)" nil t)
      (setq output-tokens (string-to-number (match-string 1))))
    (goto-char (point-max))
    (when (re-search-backward
           "\"cached_tokens\"[ \t\n]*:[ \t\n]*\\([0-9]+\\)" nil t)
      (setq cached-tokens (string-to-number (match-string 1))))
    (goto-char (point-max))
    (when (re-search-backward
           "\"model\"[ \t\n]*:[ \t\n]*\"\\([^\"]+\\)\"" nil t)
      (setq model-sym (intern-soft (match-string 1))))
    (when (and input-tokens output-tokens)
      (list :input-tokens input-tokens
            :output-tokens output-tokens
            :model model-sym
            :cached-tokens (or cached-tokens 0)))))

(defun gptel-plus--extract-tokens-gemini ()
  "Extract token counts from a Gemini response in the log buffer.
Finds the last `usageMetadata' block, which has cumulative totals."
  (goto-char (point-max))
  (let (input-tokens output-tokens)
    (when (re-search-backward
           "\"promptTokenCount\"[ \t\n]*:[ \t\n]*\\([0-9]+\\)" nil t)
      (setq input-tokens (string-to-number (match-string 1))))
    (goto-char (point-max))
    (when (re-search-backward
           "\"candidatesTokenCount\"[ \t\n]*:[ \t\n]*\\([0-9]+\\)" nil t)
      (setq output-tokens (string-to-number (match-string 1))))
    (when (and input-tokens output-tokens)
      (list :input-tokens input-tokens
            :output-tokens output-tokens
            :model nil))))

(defun gptel-plus--compute-request-cost (tokens backend-type input-cost-per-1m output-cost-per-1m)
  "Compute the raw cost of a request before normalization.
TOKENS is a plist from the extraction functions.
BACKEND-TYPE is `anthropic', `openai', or `gemini'.
INPUT-COST-PER-1M and OUTPUT-COST-PER-1M are per-million-token rates."
  (let ((input-tokens (plist-get tokens :input-tokens))
        (output-tokens (plist-get tokens :output-tokens)))
    (+ (* input-tokens input-cost-per-1m)
       (* output-tokens output-cost-per-1m)
       (pcase backend-type
         ('anthropic
          (+ (* (or (plist-get tokens :cache-creation-tokens) 0)
                input-cost-per-1m 1.25)
             (* (or (plist-get tokens :cache-read-tokens) 0)
                input-cost-per-1m 0.1)))
         ('openai
          (* (or (plist-get tokens :cached-tokens) 0)
             input-cost-per-1m -0.5))
         (_ 0)))))

(defun gptel-plus-calculate-exact-cost (&rest _)
  "Calculate and report the exact cost of the last `gptel' request.
Supports Anthropic, OpenAI, and Gemini backends.  Accounts for
prompt caching (Anthropic and OpenAI)."
  (unwind-protect
      (when gptel-plus-calculate-cost
        (condition-case err
            (when-let* ((log-buffer (get-buffer "*gptel-log*")))
              (let ((backend-type (gptel-plus--backend-type))
                    (current-model gptel-model)
                    (log-start (or gptel-plus--log-start-position 1)))
                (with-current-buffer log-buffer
                  (save-restriction
                    (narrow-to-region (min log-start (point-max)) (point-max))
                    (when-let* ((tokens (gptel-plus--extract-request-tokens backend-type))
                                (input-tokens (plist-get tokens :input-tokens))
                                (output-tokens (plist-get tokens :output-tokens))
                                (model-sym (or (plist-get tokens :model) current-model))
                                (effective-model
                                 (if (get model-sym :input-cost) model-sym current-model))
                                (input-cost-per-1m (get effective-model :input-cost))
                                (output-cost-per-1m (get effective-model :output-cost)))
                      (message "Cost of request: $%.4f"
                               (gptel-plus-normalize-cost
                                (gptel-plus--compute-request-cost
                                 tokens backend-type
                                 input-cost-per-1m output-cost-per-1m))))))))
          (error (message "gptel-plus: failed to calculate cost: %s" (error-message-string err)))))
    (cl-decf gptel-plus--logging-requests-count)
    (when (<= gptel-plus--logging-requests-count 0)
      (setq gptel-log-level gptel-plus--original-log-level)
      (when-let* ((log-buffer (get-buffer "*gptel-log*")))
        (kill-buffer log-buffer))
      (setq gptel-plus--logging-requests-count 0))))

(add-hook 'gptel-post-response-functions #'gptel-plus-calculate-exact-cost 100)

(defun gptel-plus-prepare-cost-calculation ()
  "Prepare for ex-post cost calculation by enabling logging."
  (when gptel-plus-calculate-cost
    (when (= gptel-plus--logging-requests-count 0)
      (setq gptel-plus--original-log-level gptel-log-level))
    (setq gptel-log-level 'info)
    (cl-incf gptel-plus--logging-requests-count)
    (setq-local gptel-plus--log-start-position
                (if-let* ((buf (get-buffer "*gptel-log*")))
                    (with-current-buffer buf (point-max))
                  1))))

(add-hook 'gptel-post-request-hook #'gptel-plus-prepare-cost-calculation)

;;;;;; Display costs

(defun gptel-plus--header-system-button ()
  "Return a propertized system prompt button for the header line."
  (propertize
   (buttonize
    (format "[Prompt: %s]"
	    (or (car-safe (rassoc gptel--system-message gptel-directives))
		(gptel--describe-directive gptel--system-message 15)))
    (lambda (&rest _) (gptel-system-prompt)))
   'mouse-face 'highlight
   'help-echo "System message for session"))

(defun gptel-plus--header-cost-button ()
  "Return a propertized cost button for the header line."
  (let* ((cost (gptel-plus-get-total-cost))
	 (cost-msg (if cost (format "[Cost: $%.2f]" cost) "[Cost: N/A]")))
    (propertize
     (buttonize cost-msg (lambda (&rest _) (gptel-menu)))
     'mouse-face 'highlight
     'help-echo (if cost
		    "Cost of the current prompt"
		  "There is no cost information available for this model"))))

(defun gptel-plus--header-context-info ()
  "Return a propertized context info button for the header line, or nil."
  (and gptel-context
       (cl-loop
	for entry in gptel-context
	if (bufferp (or (car-safe entry) entry)) count it into bufs
	else count (stringp (or (car-safe entry) entry)) into files
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

(defun gptel-plus--header-media-button ()
  "Return a propertized media tracking button for the header line, or nil."
  (and (gptel--model-capable-p 'media)
       (let ((toggle (lambda (&rest _)
		       (setq-local gptel-track-media (not gptel-track-media))
		       (if gptel-track-media
			   (progn
			     (run-hooks 'gptel-refresh-buffer-hook)
			     (message "Sending media from included links."))
			 (without-restriction (gptel--annotate-link-clear))
			 (message "Ignoring links.  Only link text will be sent."))
		       (run-at-time 0 nil #'force-mode-line-update))))
	 (if gptel-track-media
	     (propertize
	      (buttonize "[Sending media]" toggle)
	      'mouse-face 'highlight
	      'help-echo "Sending media from links/urls when supported.\nClick to toggle")
	   (propertize
	    (buttonize "[Ignoring media]" toggle)
	    'mouse-face 'highlight
	    'help-echo "Ignoring media from links/urls.\nClick to toggle")))))

(defun gptel-plus--header-tools-button ()
  "Return a propertized tools button for the header line, or nil."
  (when (and gptel-use-tools gptel-tools)
    (let ((toggle (lambda (&rest _) (interactive)
		    (run-at-time 0 nil
				 (lambda () (call-interactively #'gptel-tools))))))
      (propertize
       (buttonize (pcase (length gptel-tools)
		    (0 "[No tools]") (1 "[1 tool]")
		    (len (format "[%d tools]" len)))
		  toggle)
       'mouse-face 'highlight
       'help-echo "Select tools"))))

(unless gptel-plus--original-header-line-info
  (setq gptel-plus--original-header-line-info gptel--header-line-info))

(setq gptel--header-line-info
      '(:eval
	(let* ((model (gptel--model-name gptel-model))
	       (system (gptel-plus--header-system-button))
	       (cost (gptel-plus--header-cost-button))
	       (context (gptel-plus--header-context-info))
	       (track-media (gptel-plus--header-media-button))
	       (tools (gptel-plus--header-tools-button)))
	  (concat
	   (propertize
            " " 'display
            `(space :align-to (- right
				 ,(+ 5 (length model) (length system)
                                     (length track-media) (length context)
				     (length cost) (length tools)))))
	   tools (and track-media " ") track-media (and context " ") context " " cost " " system " "
	   (propertize
            (buttonize (concat "[" model "]")
                       (lambda (&rest _) (gptel-menu)))
            'mouse-face 'highlight
            'help-echo "Model in use")))))

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
  "Enable `gptel-mode' in any buffer with `gptel' data."
  (let ((was-modified (buffer-modified-p)))
    (gptel-mode)
    ;; `breadcrumb-mode' interferes with the `gptel' header line
    (when (bound-and-true-p breadcrumb-mode)
      (breadcrumb-mode -1))
    ;; Prevent the buffer from becoming modified merely because `gptel-mode'
    ;; is enabled.
    (unless was-modified
      (set-buffer-modified-p nil))))

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
	(if (derived-mode-p 'org-mode)
	    (gptel-plus-save-file-context-in-org)
	  (gptel-plus-save-file-context-in-markdown))
	(message "Saved `gptel' context: %s" (prin1-to-string gptel-context)))
    (user-error "Not in an Org or Markdown buffer")))

(defun gptel-plus-save-file-context-in-org ()
  "Save the current `gptel' file context in file visited by the current Org buffer."
  (save-excursion
    (goto-char (point-min))
    (org-set-property "GPTEL_CONTEXT" (prin1-to-string gptel-context))))

(defun gptel-plus-save-file-context-in-markdown ()
  "Save the current `gptel' file context in file visited by the current MD buffer."
  (add-file-local-variable 'gptel-plus-context (format "%S" gptel-context)))

;;;;;; Get saved

(defun gptel-plus--safe-read (str)
  "Read an Elisp object from STR, rejecting dangerous reader macros.
Signals an error if STR contains the `#.' reader macro, which would
cause arbitrary code execution at read time."
  (when (string-match-p "#\\." str)
    (error "Refusing to read data containing `#.' reader macro"))
  (car (read-from-string str)))

(defun gptel-plus-get-saved-context ()
  "Get the saved `gptel' context from the file visited by the current buffer."
  (cond
   ((derived-mode-p 'org-mode)
    (when-let* ((gptel-context-prop (org-entry-get (point-min) "GPTEL_CONTEXT")))
      (gptel-plus--safe-read gptel-context-prop)))
   ((derived-mode-p 'markdown-mode)
    (when (stringp gptel-plus-context)
      (gptel-plus--safe-read gptel-plus-context)))
   (t (user-error "Not in an Org or Markdown buffer"))))

;;;;;; Restore

(defun gptel-plus-restore-file-context ()
  "Restore the saved file context from the file visited by the current buffer."
  (interactive)
  (if-let* ((context (gptel-plus-get-saved-context)))
      (when (or (not gptel-context)
		(y-or-n-p "Overwrite current `gptel' context? "))
	(gptel-context-remove-all)
	(let (missing)
	  (dolist (entry context)
	    (let ((file (car entry)))
	      (if (and (stringp file) (file-readable-p file))
		  (gptel-context-add-file file)
		(push file missing))))
	  (when missing
	    (message "Skipped %d missing file(s): %s"
		     (length missing)
		     (mapconcat #'identity missing ", ")))))
    (message "No saved `gptel' context found.")))

;;;;; Context management

(defun gptel-plus-list-context-files ()
  "List all files in the current `gptel' context sorted by size.
Each file is shown along with its size."
  (interactive)
  (if gptel-context
      (with-current-buffer (get-buffer-create "*gptel context files*")
        (gptel-context-files-mode)
        (gptel-plus-list-context-files-internal)
        (pop-to-buffer (current-buffer)))
    (message "No files in context.")))

(defun gptel-plus-list-context-files-internal ()
  "Populate the current buffer with the gptel context files in a flaggable format.
Lists key bindings dynamically based on the current mode's keymap."
  (let* ((key-bindings (gptel-plus--describe-keybindings (current-local-map)))
         (files (cl-remove-if-not #'stringp (mapcar #'car gptel-context)))
         (file-sizes (mapcar (lambda (f)
                               (cons f (or (file-attribute-size (file-attributes f)) 0)))
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
     (define-key map (kbd "q") #'kill-current-buffer)
     map))
  (read-only-mode 1))

(defun gptel-plus--describe-keybindings (keymap)
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
    (mapconcat #'identity (sort bindings #'string<) "\n")))

(defun gptel-plus-toggle-mark ()
  "Toggle the mark on the current line's file entry and move to the next entry."
  (interactive)
  (let ((line-start (line-beginning-position)))
    (when-let* ((file (get-text-property line-start 'gptel-context-file)))
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
`gptel-flag' is non-nil, removes those files from `gptel-context',
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
          (dolist (file files-to-remove)
            (gptel-context-remove file))
          (message "Removed flagged files from context: %s"
                   (mapconcat #'identity files-to-remove ", "))
          (gptel-plus-refresh-context-files-buffer))
      (message "No files flagged for removal."))))

(defun gptel-plus-refresh-context-files-buffer ()
  "Refresh the buffer showing the gptel context-files list."
  (interactive)
  (when-let* ((buf (get-buffer "*gptel context files*")))
    (with-current-buffer buf
      (gptel-plus-list-context-files-internal)
      (message "Context file listing refreshed."))))

(defun gptel-plus-unload-function ()
  "Remove gptel-plus advice, hooks, and cleanup state.
Called automatically by `unload-feature'."
  (advice-remove 'gptel-context-add-file #'gptel-plus-update-context-cost)
  (advice-remove 'gptel-context-remove #'gptel-plus-update-context-cost)
  (advice-remove 'gptel--set-with-scope #'gptel-plus--update-cost-on-model-change)
  (advice-remove 'gptel-send #'gptel-plus-confirm-when-costs-high)
  (advice-remove 'gptel--request-data #'gptel-plus--add-stream-options)
  (advice-remove 'gptel--parse-response #'gptel-plus--capture-usage)
  (remove-hook 'gptel-post-response-functions #'gptel-plus-calculate-exact-cost)
  (remove-hook 'gptel-post-request-hook #'gptel-plus-prepare-cost-calculation)
  (when gptel-plus--original-header-line-info
    (setq gptel--header-line-info gptel-plus--original-header-line-info))
  nil)

(provide 'gptel-plus)
;;; gptel-plus.el ends here

