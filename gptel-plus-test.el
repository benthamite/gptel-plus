;;; gptel-plus-test.el --- Tests for gptel-plus -*- lexical-binding: t -*-

;; Copyright (C) 2026 Pablo Stafforini

;;; Commentary:

;; ERT test suite for gptel-plus.  Covers cost estimation (ex ante and ex
;; post), security (safe-read), context persistence, auto-mode detection,
;; context file management, header line helpers, and concurrent request
;; management.

;;; Code:

(require 'ert)
(require 'gptel-plus)

;;;; Helpers

(defvar gptel-plus-test--model 'test-model
  "Mock model symbol for tests.")

(defmacro gptel-plus-test-with-model (input-cost output-cost &rest body)
  "Execute BODY with a mock model that has INPUT-COST and OUTPUT-COST per 1M tokens.
Binds `gptel-model' to the mock model and sets symbol properties."
  (declare (indent 2))
  `(let ((gptel-model gptel-plus-test--model)
         (gptel-plus-calculate-cost t)
         (gptel-plus-tokens-per-word 1.5)
         (gptel-plus-tokens-in-output 100)
         (gptel-plus--context-cost nil))
     (put gptel-plus-test--model :input-cost ,input-cost)
     (put gptel-plus-test--model :output-cost ,output-cost)
     (unwind-protect
         (progn ,@body)
       (setplist gptel-plus-test--model nil))))

(defmacro gptel-plus-test-with-temp-buffer (contents &rest body)
  "Insert CONTENTS into a temp buffer, go to end, then execute BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,contents)
     ,@body))

(defun gptel-plus-test--make-log-buffer (events)
  "Create a `*gptel-log*' buffer with EVENTS, a list of (event-type . data-json) pairs."
  (let ((buf (get-buffer-create "*gptel-log*")))
    (with-current-buffer buf
      (erase-buffer)
      (dolist (ev events)
        (insert (format "event: %s\ndata: %s\n\n" (car ev) (cdr ev)))))
    buf))

(defun gptel-plus-test--make-temp-file (content)
  "Create a temp file with CONTENT, return its path."
  (let ((f (make-temp-file "gptel-plus-test-")))
    (with-temp-file f (insert content))
    f))

;;;; Cost normalization

(ert-deftest gptel-plus-test-normalize-cost-basic ()
  "Dividing by 1M converts cost-per-million to dollars."
  (should (= (gptel-plus-normalize-cost 1000000.0) 1.0))
  (should (= (gptel-plus-normalize-cost 500000.0) 0.5))
  (should (= (gptel-plus-normalize-cost 0.0) 0.0)))

(ert-deftest gptel-plus-test-normalize-cost-small-values ()
  "Sub-cent costs are represented accurately."
  (should (< (abs (- (gptel-plus-normalize-cost 3.0) 0.000003)) 1e-12)))

(ert-deftest gptel-plus-test-normalize-cost-returns-float ()
  "Result is always a float, even with integer input."
  (should (floatp (gptel-plus-normalize-cost 100))))

;;;; Word counting

(ert-deftest gptel-plus-test-count-words-in-buffer-whole ()
  "Counts words from point-min to point (simulating gptel send behavior)."
  (gptel-plus-test-with-temp-buffer "one two three four five"
    ;; point is at end after insert
    (should (= (gptel-plus-count-words-in-buffer) 5))))

(ert-deftest gptel-plus-test-count-words-in-buffer-partial ()
  "Only counts words before point."
  (gptel-plus-test-with-temp-buffer "one two three four five"
    (goto-char (point-min))
    (forward-word 2)
    (should (= (gptel-plus-count-words-in-buffer) 2))))

(ert-deftest gptel-plus-test-count-words-in-buffer-empty ()
  "Empty buffer returns zero words."
  (with-temp-buffer
    (should (= (gptel-plus-count-words-in-buffer) 0))))

(ert-deftest gptel-plus-test-count-words-in-buffer-region ()
  "When region is active, counts only words in the region."
  (gptel-plus-test-with-temp-buffer "one two three four five"
    (goto-char (point-min))
    (push-mark nil t t)
    (forward-word 3)
    (should (= (gptel-plus-count-words-in-buffer) 3))
    (deactivate-mark)))

(ert-deftest gptel-plus-test-count-words-in-context-files ()
  "Counts words across multiple files in context."
  (let ((f1 (gptel-plus-test--make-temp-file "hello world"))
        (f2 (gptel-plus-test--make-temp-file "one two three four"))
        (gptel-context nil))
    (unwind-protect
        (progn
          (setq gptel-context (list (list f1) (list f2)))
          (should (= (gptel-plus-count-words-in-context) 6)))
      (delete-file f1)
      (delete-file f2))))

(ert-deftest gptel-plus-test-count-words-in-context-buffer ()
  "Counts words in buffer entries in context."
  (let ((gptel-context nil))
    (with-temp-buffer
      (insert "alpha beta gamma")
      (setq gptel-context (list (list (current-buffer))))
      (should (= (gptel-plus-count-words-in-context) 3)))))

(ert-deftest gptel-plus-test-count-words-in-context-empty ()
  "Empty context returns zero."
  (let ((gptel-context nil))
    (should (= (gptel-plus-count-words-in-context) 0))))

(ert-deftest gptel-plus-test-count-words-in-context-dead-buffer ()
  "Dead buffer entries contribute zero words."
  (let ((gptel-context nil)
        buf)
    (setq buf (generate-new-buffer " *test-dead*"))
    (with-current-buffer buf (insert "should not count"))
    (setq gptel-context (list (list buf)))
    (kill-buffer buf)
    (should (= (gptel-plus-count-words-in-context) 0))))

(ert-deftest gptel-plus-test-count-words-in-context-missing-file ()
  "Unreadable file entries contribute zero words."
  (let ((gptel-context (list (list "/nonexistent/file.txt"))))
    (should (= (gptel-plus-count-words-in-context) 0))))

(ert-deftest gptel-plus-test-count-words-in-context-mixed ()
  "Context with buffers, files, dead buffers, and missing files totals correctly."
  (let ((f (gptel-plus-test--make-temp-file "file words here"))
        (gptel-context nil)
        live-buf dead-buf)
    (setq live-buf (generate-new-buffer " *test-live*"))
    (with-current-buffer live-buf (insert "live buffer"))
    (setq dead-buf (generate-new-buffer " *test-dead*"))
    (kill-buffer dead-buf)
    (unwind-protect
        (progn
          (setq gptel-context (list (list live-buf)
                                    (list f)
                                    (list dead-buf)
                                    (list "/no/such/file")))
          ;; live-buf: 2, file: 3, dead-buf: 0, missing: 0 = 5
          (should (= (gptel-plus-count-words-in-context) 5)))
      (when (buffer-live-p live-buf) (kill-buffer live-buf))
      (delete-file f))))

;;;; Ex ante cost estimation

(ert-deftest gptel-plus-test-get-output-cost ()
  "Output cost = output-cost-per-1M * tokens-in-output."
  (gptel-plus-test-with-model 3.0 15.0
    (should (= (gptel-plus-get-output-cost) (* 15.0 100)))))

(ert-deftest gptel-plus-test-get-output-cost-nil-when-no-pricing ()
  "Returns nil when model has no output cost property."
  (let ((gptel-model 'model-without-pricing)
        (gptel-plus-tokens-in-output 100))
    (should (null (gptel-plus-get-output-cost)))))

(ert-deftest gptel-plus-test-get-buffer-cost ()
  "Buffer cost = input-cost-per-1M * tokens-per-word * word-count."
  (gptel-plus-test-with-model 3.0 15.0
    (gptel-plus-test-with-temp-buffer "one two three four five six seven eight nine ten"
      ;; 10 words * 1.5 tokens/word * 3.0 = 45.0
      (should (= (gptel-plus-get-buffer-cost) 45.0)))))

(ert-deftest gptel-plus-test-get-context-cost ()
  "Context cost = input-cost-per-1M * tokens-per-word * context-word-count."
  (let ((f (gptel-plus-test--make-temp-file "alpha beta gamma delta")))
    (unwind-protect
        (gptel-plus-test-with-model 3.0 15.0
          (let ((gptel-context (list (list f))))
            ;; 4 words * 1.5 * 3.0 = 18.0
            (should (= (gptel-plus-get-context-cost) 18.0))))
      (delete-file f))))

(ert-deftest gptel-plus-test-get-input-cost-combines-buffer-and-context ()
  "Input cost sums buffer cost and cached context cost."
  (gptel-plus-test-with-model 3.0 15.0
    (gptel-plus-test-with-temp-buffer "one two"
      ;; buffer: 2 words * 1.5 * 3.0 = 9.0
      ;; context cache: 18.0
      (setq gptel-plus--context-cost 18.0)
      (should (= (gptel-plus-get-input-cost) 27.0)))))

(ert-deftest gptel-plus-test-get-input-cost-no-context ()
  "Input cost equals buffer cost when context cache is nil."
  (gptel-plus-test-with-model 3.0 15.0
    (gptel-plus-test-with-temp-buffer "one two"
      ;; buffer: 2 * 1.5 * 3.0 = 9.0, context nil -> 0
      (should (= (gptel-plus-get-input-cost) 9.0)))))

(ert-deftest gptel-plus-test-get-total-cost ()
  "Total cost = normalize(input-cost + output-cost)."
  (gptel-plus-test-with-model 3.0 15.0
    (gptel-plus-test-with-temp-buffer "word"
      ;; input: 1 word * 1.5 * 3.0 = 4.5 (buffer) + 0 (context) = 4.5
      ;; output: 15.0 * 100 = 1500.0
      ;; total raw: 1504.5
      ;; normalized: 1504.5 / 1000000.0 = 0.0015045
      (should (< (abs (- (gptel-plus-get-total-cost) 0.0015045)) 1e-10)))))

(ert-deftest gptel-plus-test-get-total-cost-nil-when-disabled ()
  "Total cost is nil when cost calculation is disabled."
  (gptel-plus-test-with-model 3.0 15.0
    (let ((gptel-plus-calculate-cost nil))
      (gptel-plus-test-with-temp-buffer "word"
        (should (null (gptel-plus-get-total-cost)))))))

(ert-deftest gptel-plus-test-get-total-cost-nil-when-no-pricing ()
  "Total cost is nil when model has no pricing info."
  (let ((gptel-model 'unpriced-model)
        (gptel-plus-calculate-cost t)
        (gptel-plus-tokens-per-word 1.5)
        (gptel-plus-tokens-in-output 100))
    (gptel-plus-test-with-temp-buffer "word"
      (should (null (gptel-plus-get-total-cost))))))

(ert-deftest gptel-plus-test-get-total-cost-realistic ()
  "Realistic cost: Claude Sonnet 4 pricing ($3/$15 per 1M tokens)."
  (gptel-plus-test-with-model 3.0 15.0
    (gptel-plus-test-with-temp-buffer
        ;; ~100 words
        "The quick brown fox jumps over the lazy dog. \
This sentence has ten words in it now. \
Another sentence here to reach our target count. \
We need about one hundred words total for this test. \
So let us keep writing more words here. \
Each sentence adds roughly eight to ten words. \
We are getting closer to our goal now. \
Just a few more sentences should do it. \
Almost there with the word count requirement. \
This final sentence puts us over one hundred words."
      ;; ~100 words * 1.5 tokens/word = 150 input tokens
      ;; input cost: 150 * 3.0 = 450
      ;; output cost: 100 * 15.0 = 1500
      ;; total raw: 1950
      ;; normalized: 1950 / 1M ≈ $0.00195
      (let ((cost (gptel-plus-get-total-cost)))
        (should cost)
        (should (< cost 0.01))   ; less than a cent
        (should (> cost 0.0))))))

(ert-deftest gptel-plus-test-update-context-cost-caches ()
  "update-context-cost stores the computed context cost."
  (let ((f (gptel-plus-test--make-temp-file "one two three")))
    (unwind-protect
        (gptel-plus-test-with-model 3.0 15.0
          (let ((gptel-context (list (list f))))
            (gptel-plus-update-context-cost)
            ;; 3 words * 1.5 * 3.0 = 13.5
            (should (= gptel-plus--context-cost 13.5))))
      (delete-file f))))

(ert-deftest gptel-plus-test-custom-tokens-per-word ()
  "Changing tokens-per-word scales cost proportionally."
  (gptel-plus-test-with-model 3.0 15.0
    (gptel-plus-test-with-temp-buffer "one two three four"
      ;; Default 1.5: 4 * 1.5 * 3.0 = 18.0
      (should (= (gptel-plus-get-buffer-cost) 18.0))
      ;; Double the ratio: 4 * 3.0 * 3.0 = 36.0
      (let ((gptel-plus-tokens-per-word 3.0))
        (should (= (gptel-plus-get-buffer-cost) 36.0))))))

(ert-deftest gptel-plus-test-custom-tokens-in-output ()
  "Changing tokens-in-output scales output cost."
  (gptel-plus-test-with-model 3.0 15.0
    (let ((gptel-plus-tokens-in-output 500))
      ;; 500 * 15.0 = 7500
      (should (= (gptel-plus-get-output-cost) 7500.0)))))

(ert-deftest gptel-plus-test-update-cost-on-model-change-triggers ()
  "Cost update triggers when symbol is gptel-model or gptel-backend."
  (let ((updated nil))
    (cl-letf (((symbol-function 'gptel-plus-update-context-cost)
               (lambda (&rest _) (setq updated t))))
      (gptel-plus--update-cost-on-model-change 'gptel-model nil)
      (should updated)
      (setq updated nil)
      (gptel-plus--update-cost-on-model-change 'gptel-backend nil)
      (should updated))))

(ert-deftest gptel-plus-test-update-cost-on-model-change-ignores-other ()
  "Cost update does NOT trigger for unrelated symbols."
  (let ((updated nil))
    (cl-letf (((symbol-function 'gptel-plus-update-context-cost)
               (lambda (&rest _) (setq updated t))))
      (gptel-plus--update-cost-on-model-change 'gptel-temperature nil)
      (should-not updated))))

;;;; Ex post cost estimation (log parsing)

(defconst gptel-plus-test--sample-message-start
  "{\"type\":\"message_start\",\"message\":{\"id\":\"msg_01\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[],\"model\":\"test-model\",\"stop_reason\":null,\"usage\":{\"input_tokens\":250,\"output_tokens\":0,\"cache_creation_input_tokens\":0,\"cache_read_input_tokens\":0}}}"
  "Sample Anthropic message_start event data.")

(defconst gptel-plus-test--sample-message-delta
  "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":75}}"
  "Sample Anthropic message_delta event data.")

(ert-deftest gptel-plus-test-read-log-event-json ()
  "Parses JSON from a 'data: ...' line in the log buffer."
  (with-temp-buffer
    (insert "event: message_start\n")
    (insert "data: {\"type\":\"message_start\",\"value\":42}\n")
    (goto-char (point-min))
    (let ((json (gptel-plus--read-log-event-json)))
      (should json)
      (should (equal (cdr (assoc 'type json)) "message_start"))
      (should (= (cdr (assoc 'value json)) 42)))))

(ert-deftest gptel-plus-test-read-log-event-json-no-data ()
  "Returns nil when there is no data line."
  (with-temp-buffer
    (insert "event: ping\n\n")
    (goto-char (point-min))
    (should (null (gptel-plus--read-log-event-json)))))

(ert-deftest gptel-plus-test-extract-output-tokens ()
  "Extracts output tokens from the last message_delta event."
  (let ((buf (gptel-plus-test--make-log-buffer
              (list (cons "message_start" gptel-plus-test--sample-message-start)
                    (cons "content_block_delta" "{\"type\":\"content_block_delta\"}")
                    (cons "message_delta" gptel-plus-test--sample-message-delta)))))
    (unwind-protect
        (with-current-buffer buf
          (let ((result (gptel-plus--extract-output-tokens)))
            (should result)
            (should (= (car result) 75))
            (should (integerp (cdr result)))))
      (kill-buffer buf))))

(ert-deftest gptel-plus-test-extract-output-tokens-no-delta ()
  "Returns nil when there is no message_delta event."
  (let ((buf (gptel-plus-test--make-log-buffer
              (list (cons "message_start" gptel-plus-test--sample-message-start)))))
    (unwind-protect
        (with-current-buffer buf
          (should (null (gptel-plus--extract-output-tokens))))
      (kill-buffer buf))))

(ert-deftest gptel-plus-test-extract-input-tokens-and-model ()
  "Extracts input tokens and model symbol from message_start event."
  (let ((buf (gptel-plus-test--make-log-buffer
              (list (cons "message_start" gptel-plus-test--sample-message-start)
                    (cons "message_delta" gptel-plus-test--sample-message-delta)))))
    (unwind-protect
        (with-current-buffer buf
          ;; Need test-model interned
          (let ((result (gptel-plus--extract-input-tokens-and-model (point-max))))
            (should result)
            (should (= (car result) 250))
            (should (eq (cdr result) 'test-model))))
      (kill-buffer buf))))

(ert-deftest gptel-plus-test-extract-input-tokens-no-start ()
  "Returns nil when there is no message_start event."
  (let ((buf (gptel-plus-test--make-log-buffer
              (list (cons "message_delta" gptel-plus-test--sample-message-delta)))))
    (unwind-protect
        (with-current-buffer buf
          (should (null (gptel-plus--extract-input-tokens-and-model (point-max)))))
      (kill-buffer buf))))

(ert-deftest gptel-plus-test-calculate-exact-cost-full-pipeline ()
  "Full ex post pipeline: parses log, computes cost, displays message."
  (let ((gptel-plus-calculate-cost t)
        (gptel-plus--logging-requests-count 1)
        (gptel-plus--original-log-level nil)
        (gptel-log-level 'info)
        (last-message nil))
    ;; Set up model pricing
    (put 'test-model :input-cost 3.0)
    (put 'test-model :output-cost 15.0)
    (gptel-plus-test--make-log-buffer
     (list (cons "message_start" gptel-plus-test--sample-message-start)
           (cons "content_block_delta" "{\"type\":\"content_block_delta\"}")
           (cons "message_delta" gptel-plus-test--sample-message-delta)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args) (setq last-message (apply #'format fmt args)))))
            (gptel-plus-calculate-exact-cost))
          ;; Exact cost: (250 * 3.0 + 75 * 15.0) / 1M = (750 + 1125) / 1M = 0.001875
          (should (string-match-p "\\$0\\.0019" last-message))
          ;; Counter decremented
          (should (= gptel-plus--logging-requests-count 0))
          ;; Log buffer killed (last request)
          (should (null (get-buffer "*gptel-log*"))))
      (setplist 'test-model nil)
      (when-let* ((buf (get-buffer "*gptel-log*")))
        (kill-buffer buf)))))

(ert-deftest gptel-plus-test-calculate-exact-cost-counter-management ()
  "Counter decrements even when calculation fails."
  (let ((gptel-plus-calculate-cost t)
        (gptel-plus--logging-requests-count 2)
        (gptel-plus--original-log-level nil)
        (gptel-log-level 'info))
    ;; No log buffer - calculation will silently skip
    (when-let* ((buf (get-buffer "*gptel-log*")))
      (kill-buffer buf))
    (cl-letf (((symbol-function 'message) #'ignore))
      (gptel-plus-calculate-exact-cost))
    ;; Counter still decremented
    (should (= gptel-plus--logging-requests-count 1))
    ;; Log level NOT restored (still have outstanding requests)
    (should (eq gptel-log-level 'info))))

(ert-deftest gptel-plus-test-extract-output-tokens-wrong-stop-reason ()
  "Returns nil when stop_reason is not end_turn."
  (let ((buf (gptel-plus-test--make-log-buffer
              (list (cons "message_delta"
                          "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"max_tokens\"},\"usage\":{\"output_tokens\":50}}")))))
    (unwind-protect
        (with-current-buffer buf
          (should (null (gptel-plus--extract-output-tokens))))
      (kill-buffer buf))))

(ert-deftest gptel-plus-test-extract-picks-last-request ()
  "When log has multiple requests, parser finds the last message_delta."
  (let* ((start1 "{\"type\":\"message_start\",\"message\":{\"id\":\"msg_01\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[],\"model\":\"test-model\",\"stop_reason\":null,\"usage\":{\"input_tokens\":100,\"output_tokens\":0}}}")
         (delta1 "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":30}}")
         (start2 "{\"type\":\"message_start\",\"message\":{\"id\":\"msg_02\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[],\"model\":\"test-model\",\"stop_reason\":null,\"usage\":{\"input_tokens\":200,\"output_tokens\":0}}}")
         (delta2 "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":50}}")
         (buf (gptel-plus-test--make-log-buffer
               (list (cons "message_start" start1)
                     (cons "message_delta" delta1)
                     (cons "message_start" start2)
                     (cons "message_delta" delta2)))))
    (unwind-protect
        (with-current-buffer buf
          ;; Should find the LAST delta (50 output tokens)
          (let ((result (gptel-plus--extract-output-tokens)))
            (should result)
            (should (= (car result) 50)))
          ;; And the corresponding start (200 input tokens)
          (let* ((output-result (gptel-plus--extract-output-tokens))
                 (input-result (gptel-plus--extract-input-tokens-and-model
                                (cdr output-result))))
            (should input-result)
            (should (= (car input-result) 200))))
      (kill-buffer buf))))

(ert-deftest gptel-plus-test-calculate-exact-cost-error-handling ()
  "Malformed log data produces error message but does not signal."
  (let ((gptel-plus-calculate-cost t)
        (gptel-plus--logging-requests-count 1)
        (gptel-plus--original-log-level nil)
        (gptel-log-level 'info)
        (last-msg nil))
    (let ((buf (get-buffer-create "*gptel-log*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert "event: message_delta\ndata: {invalid json!}\n")))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args) (setq last-msg (apply #'format fmt args)))))
            ;; Should not signal - errors are caught
            (gptel-plus-calculate-exact-cost))
          ;; Error message displayed
          (should (string-match-p "failed to calculate" last-msg))
          ;; Counter still decremented (unwind-protect)
          (should (= gptel-plus--logging-requests-count 0)))
      (when-let* ((buf (get-buffer "*gptel-log*")))
        (kill-buffer buf)))))

(ert-deftest gptel-plus-test-calculate-exact-cost-disabled ()
  "When cost calculation is disabled, still decrements counter and cleans up."
  (let ((gptel-plus-calculate-cost nil)
        (gptel-plus--logging-requests-count 1)
        (gptel-plus--original-log-level nil)
        (gptel-log-level 'info))
    (gptel-plus-test--make-log-buffer
     (list (cons "message_start" gptel-plus-test--sample-message-start)
           (cons "message_delta" gptel-plus-test--sample-message-delta)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'message) #'ignore))
            (gptel-plus-calculate-exact-cost))
          (should (= gptel-plus--logging-requests-count 0)))
      (when-let* ((buf (get-buffer "*gptel-log*")))
        (kill-buffer buf)))))

(ert-deftest gptel-plus-test-prepare-cost-calculation ()
  "Prepare saves original log level and increments counter."
  (let ((gptel-plus-calculate-cost t)
        (gptel-plus--logging-requests-count 0)
        (gptel-plus--original-log-level nil)
        (gptel-log-level nil))
    (gptel-plus-prepare-cost-calculation)
    (should (= gptel-plus--logging-requests-count 1))
    (should (null gptel-plus--original-log-level))
    (should (eq gptel-log-level 'info))))

(ert-deftest gptel-plus-test-prepare-cost-calculation-concurrent ()
  "Second prepare increments counter but preserves original log level."
  (let ((gptel-plus-calculate-cost t)
        (gptel-plus--logging-requests-count 1)
        (gptel-plus--original-log-level 'warn)
        (gptel-log-level 'info))
    (gptel-plus-prepare-cost-calculation)
    ;; Counter incremented
    (should (= gptel-plus--logging-requests-count 2))
    ;; Original level NOT overwritten (was saved on first prepare)
    (should (eq gptel-plus--original-log-level 'warn))))

(ert-deftest gptel-plus-test-prepare-cost-calculation-disabled ()
  "When cost calculation is disabled, prepare does nothing."
  (let ((gptel-plus-calculate-cost nil)
        (gptel-plus--logging-requests-count 0)
        (gptel-log-level nil))
    (gptel-plus-prepare-cost-calculation)
    (should (= gptel-plus--logging-requests-count 0))
    (should (null gptel-log-level))))

;;;; Security: safe-read

(ert-deftest gptel-plus-test-safe-read-normal-list ()
  "Reads a normal list safely."
  (should (equal (gptel-plus--safe-read "((\"a\" . 1) (\"b\" . 2))")
                 '(("a" . 1) ("b" . 2)))))

(ert-deftest gptel-plus-test-safe-read-file-paths ()
  "Reads file path context data safely."
  (let ((data "(\"/home/user/file.el\" \"/tmp/other.txt\")"))
    (should (equal (gptel-plus--safe-read data)
                   '("/home/user/file.el" "/tmp/other.txt")))))

(ert-deftest gptel-plus-test-safe-read-rejects-reader-macro ()
  "Signals error on #. reader macro (code execution vector)."
  (should-error (gptel-plus--safe-read "#.(delete-file \"/etc/passwd\")")
                :type 'error))

(ert-deftest gptel-plus-test-safe-read-rejects-embedded-reader-macro ()
  "Signals error when #. appears anywhere in the string."
  (should-error (gptel-plus--safe-read "((\"safe\") #.(evil))")
                :type 'error))

(ert-deftest gptel-plus-test-safe-read-allows-hash-without-dot ()
  "Strings containing # without . are safe."
  (should (equal (gptel-plus--safe-read "(\"file#backup\")")
                 '("file#backup"))))

(ert-deftest gptel-plus-test-safe-read-empty-list ()
  "Reads empty list."
  (should (null (gptel-plus--safe-read "nil"))))

;;;; Auto-mode detection

(ert-deftest gptel-plus-test-file-has-gptel-local-variable-yes ()
  "Detects when gptel local variables are present."
  (with-temp-buffer
    (setq-local gptel-mode t)
    (should (gptel-plus-file-has-gptel-local-variable-p))))

(ert-deftest gptel-plus-test-file-has-gptel-local-variable-no ()
  "Returns nil when no gptel local variables are set."
  (with-temp-buffer
    (should-not (gptel-plus-file-has-gptel-local-variable-p))))

(ert-deftest gptel-plus-test-file-has-gptel-local-variable-backend ()
  "Detects gptel--backend-name local variable."
  (with-temp-buffer
    (setq-local gptel--backend-name "Anthropic")
    (should (gptel-plus-file-has-gptel-local-variable-p))))

(ert-deftest gptel-plus-test-file-has-gptel-org-property-yes ()
  "Detects gptel Org properties in a buffer."
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:GPTEL_MODEL: claude-sonnet-4-5-20250514\n:END:\n")
    (should (gptel-plus-file-has-gptel-org-property-p))))

(ert-deftest gptel-plus-test-file-has-gptel-org-property-no ()
  "Returns nil in an Org buffer without gptel properties."
  (with-temp-buffer
    (org-mode)
    (insert "* Just a heading\nSome text.\n")
    (should-not (gptel-plus-file-has-gptel-org-property-p))))

(ert-deftest gptel-plus-test-enable-gptel-common-preserves-modified ()
  "Enabling gptel-mode does not mark an unmodified buffer as modified."
  (with-temp-buffer
    (insert "content")
    (set-buffer-modified-p nil)
    ;; Stub gptel-mode to avoid needing full gptel initialization
    (cl-letf (((symbol-function 'gptel-mode) (lambda () (setq-local gptel-mode t))))
      (gptel-plus-enable-gptel-common))
    (should-not (buffer-modified-p))))

(ert-deftest gptel-plus-test-enable-gptel-common-preserves-modified-state ()
  "If buffer was already modified, it stays modified."
  (with-temp-buffer
    (insert "content")
    (set-buffer-modified-p t)
    (cl-letf (((symbol-function 'gptel-mode) (lambda () (setq-local gptel-mode t))))
      (gptel-plus-enable-gptel-common))
    (should (buffer-modified-p))))

;;;; Context persistence

(ert-deftest gptel-plus-test-get-saved-context-errors-in-wrong-mode ()
  "Signals user-error when not in Org or Markdown mode."
  (with-temp-buffer
    (fundamental-mode)
    (should-error (gptel-plus-get-saved-context)
                  :type 'user-error)))

(ert-deftest gptel-plus-test-save-context-errors-in-wrong-mode ()
  "Signals user-error when saving outside Org/Markdown."
  (with-temp-buffer
    (fundamental-mode)
    (should-error (gptel-plus-save-file-context)
                  :type 'user-error)))

(ert-deftest gptel-plus-test-get-saved-context-markdown-round-trip ()
  "Context round-trips through safe-read in markdown mode."
  (with-temp-buffer
    (let ((major-mode 'markdown-mode)
          (gptel-plus-context "((\"/tmp/a.el\") (\"/tmp/b.el\"))"))
      (let ((result (gptel-plus-get-saved-context)))
        (should (equal result '(("/tmp/a.el") ("/tmp/b.el"))))))))

(ert-deftest gptel-plus-test-get-saved-context-markdown-nil ()
  "Returns nil when no context is saved in markdown."
  (with-temp-buffer
    (let ((major-mode 'markdown-mode)
          (gptel-plus-context nil))
      (should (null (gptel-plus-get-saved-context))))))

(ert-deftest gptel-plus-test-restore-context-missing-files ()
  "Restore skips missing files and reports them."
  (let* ((existing (gptel-plus-test--make-temp-file "exists"))
         (gptel-context nil)
         (last-msg nil))
    (unwind-protect
        (with-temp-buffer
          (let ((major-mode 'markdown-mode)
                (gptel-plus-context (format "((\"%s\") (\"/nonexistent/missing.el\"))"
                                           existing)))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args) (setq last-msg (apply #'format fmt args))))
                      ((symbol-function 'gptel-context-remove-all) #'ignore)
                      ((symbol-function 'gptel-context-add-file)
                       (lambda (f) (push (list f) gptel-context))))
              (gptel-plus-restore-file-context)
              ;; Only the existing file was added
              (should (= (length gptel-context) 1))
              (should (equal (caar gptel-context) existing))
              ;; Warning about missing file
              (should (string-match-p "missing" last-msg)))))
      (delete-file existing))))

;;;; Context file management UI

(ert-deftest gptel-plus-test-toggle-mark ()
  "Toggle mark flips the flag and changes the marker text."
  (let ((gptel-context '(("/tmp/test.el"))))
    (with-temp-buffer
      (gptel-context-files-mode)
      (let ((inhibit-read-only t))
        (insert "[ ]\t1.00 KB\t/tmp/test.el\n")
        (put-text-property 1 4 'gptel-context-file "/tmp/test.el")
        (put-text-property 1 4 'gptel-flag nil))
      (goto-char (point-min))
      (gptel-plus-toggle-mark)
      ;; After toggle: flag should be t, marker should be [X]
      (should (get-text-property 1 'gptel-flag))
      (goto-char (point-min))
      (should (looking-at "\\[X\\]"))
      ;; Toggle back
      (goto-char (point-min))
      (gptel-plus-toggle-mark)
      (should-not (get-text-property 1 'gptel-flag))
      (goto-char (point-min))
      (should (looking-at "\\[ \\]")))))

(ert-deftest gptel-plus-test-remove-flagged-context-files ()
  "Flagged files are removed from gptel-context."
  (let* ((file-a "/tmp/file-a.el")
         (file-b "/tmp/file-b.el")
         (gptel-context (list (list file-a) (list file-b)))
         (gptel-plus--context-cost nil)
         (last-msg nil))
    (with-current-buffer (get-buffer-create "*gptel context files*")
      (gptel-context-files-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Context files (sorted by size):\n\n\n\n")
        ;; file-a is flagged
        (let ((start (point)))
          (insert (format "[X]\t1.00 KB\t%s\n" file-a))
          (put-text-property start (+ start 3) 'gptel-context-file file-a)
          (put-text-property start (+ start 3) 'gptel-flag t))
        ;; file-b is not flagged
        (let ((start (point)))
          (insert (format "[ ]\t2.00 KB\t%s\n" file-b))
          (put-text-property start (+ start 3) 'gptel-context-file file-b)
          (put-text-property start (+ start 3) 'gptel-flag nil)))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq last-msg (apply #'format fmt args))))
                ((symbol-function 'gptel-plus-refresh-context-files-buffer) #'ignore))
        (gptel-plus-remove-flagged-context-files))
      ;; file-a removed, file-b remains
      (should (= (length gptel-context) 1))
      (should (equal (caar gptel-context) file-b))
      (should (string-match-p "file-a" last-msg))
      (kill-buffer))))

(ert-deftest gptel-plus-test-remove-flagged-no-flags ()
  "No-op when nothing is flagged."
  (let ((gptel-context (list (list "/tmp/keep.el")))
        (last-msg nil))
    (with-current-buffer (get-buffer-create "*gptel context files*")
      (gptel-context-files-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let ((start (point)))
          (insert "[ ]\t1.00 KB\t/tmp/keep.el\n")
          (put-text-property start (+ start 3) 'gptel-context-file "/tmp/keep.el")
          (put-text-property start (+ start 3) 'gptel-flag nil)))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq last-msg (apply #'format fmt args)))))
        (gptel-plus-remove-flagged-context-files))
      (should (= (length gptel-context) 1))
      (should (string-match-p "No files flagged" last-msg))
      (kill-buffer))))

;;;; Header line helpers

(ert-deftest gptel-plus-test-header-cost-button-with-cost ()
  "Cost button shows dollar amount when pricing is available."
  (gptel-plus-test-with-model 3.0 15.0
    (gptel-plus-test-with-temp-buffer "hello world"
      (let ((btn (gptel-plus--header-cost-button)))
        (should (string-match-p "\\[Cost: \\$" btn))))))

(ert-deftest gptel-plus-test-header-cost-button-no-pricing ()
  "Cost button shows N/A when pricing is unavailable."
  (let ((gptel-model 'no-price-model)
        (gptel-plus-calculate-cost t)
        (gptel-plus-tokens-per-word 1.5)
        (gptel-plus-tokens-in-output 100)
        (gptel-plus--context-cost nil))
    (with-temp-buffer
      (insert "test")
      (let ((btn (gptel-plus--header-cost-button)))
        (should (string-match-p "N/A" btn))))))

(ert-deftest gptel-plus-test-header-context-info-nil-when-empty ()
  "Context info returns nil when there is no context."
  (let ((gptel-context nil))
    (should (null (gptel-plus--header-context-info)))))

(ert-deftest gptel-plus-test-header-context-info-counts ()
  "Context info counts buffers and files separately."
  (let ((buf (generate-new-buffer " *test-ctx*")))
    (unwind-protect
        (let ((gptel-context (list (list buf)
                                   (list "/tmp/a.el")
                                   (list "/tmp/b.el"))))
          (let ((info (gptel-plus--header-context-info)))
            (should info)
            ;; 1 buffer, 2 files
            (should (string-match-p "1 buf" info))
            (should (string-match-p "2 files" info))))
      (kill-buffer buf))))

(ert-deftest gptel-plus-test-header-context-info-single-file ()
  "Singular 'file' not 'files' for single file."
  (let ((gptel-context (list (list "/tmp/only.el"))))
    (let ((info (gptel-plus--header-context-info)))
      (should info)
      (should (string-match-p "1 file\\]" info))
      (should-not (string-match-p "files" info)))))

(ert-deftest gptel-plus-test-header-context-info-only-buffers ()
  "Context with only buffers shows buffer count without file section."
  (let ((buf1 (generate-new-buffer " *ctx-1*"))
        (buf2 (generate-new-buffer " *ctx-2*")))
    (unwind-protect
        (let ((gptel-context (list (list buf1) (list buf2))))
          (let ((info (gptel-plus--header-context-info)))
            (should info)
            (should (string-match-p "2 bufs" info))
            (should-not (string-match-p "file" info))))
      (kill-buffer buf1)
      (kill-buffer buf2))))

(ert-deftest gptel-plus-test-header-cost-button-cost-disabled ()
  "Cost button shows N/A when cost calculation is disabled."
  (gptel-plus-test-with-model 3.0 15.0
    (let ((gptel-plus-calculate-cost nil))
      (with-temp-buffer
        (insert "test")
        (let ((btn (gptel-plus--header-cost-button)))
          (should (string-match-p "N/A" btn)))))))

(ert-deftest gptel-plus-test-header-tools-button-nil-when-no-tools ()
  "Tools button returns nil when tools are disabled."
  (let ((gptel-use-tools nil)
        (gptel-tools nil))
    (should (null (gptel-plus--header-tools-button)))))

(ert-deftest gptel-plus-test-header-tools-button-count ()
  "Tools button shows correct count."
  (let ((gptel-use-tools t)
        (gptel-tools '(tool-a tool-b tool-c)))
    (let ((btn (gptel-plus--header-tools-button)))
      (should btn)
      (should (string-match-p "3 tools" btn)))))

(ert-deftest gptel-plus-test-header-tools-button-singular ()
  "Tools button uses singular for one tool."
  (let ((gptel-use-tools t)
        (gptel-tools '(tool-a)))
    (let ((btn (gptel-plus--header-tools-button)))
      (should btn)
      (should (string-match-p "1 tool\\]" btn))
      (should-not (string-match-p "tools" btn)))))

;;;; Describe keybindings

(ert-deftest gptel-plus-test-describe-keybindings ()
  "Formats keymap bindings as 'key = command' strings."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "x") #'forward-char)
    (define-key map (kbd "q") #'backward-char)
    (let ((desc (gptel-plus--describe-keybindings map)))
      (should (string-match-p "x = forward-char" desc))
      (should (string-match-p "q = backward-char" desc)))))

;;;; List context files

(ert-deftest gptel-plus-test-list-context-files-no-context ()
  "Prints message when context is empty."
  (let ((gptel-context nil)
        (last-msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq last-msg (apply #'format fmt args)))))
      (gptel-plus-list-context-files)
      (should (string-match-p "No files" last-msg)))))

(ert-deftest gptel-plus-test-list-context-files-internal-sorting ()
  "Files are sorted by size descending."
  (let* ((small (gptel-plus-test--make-temp-file "hi"))
         (large (gptel-plus-test--make-temp-file (make-string 10000 ?x)))
         (gptel-context (list (list small) (list large))))
    (unwind-protect
        (with-temp-buffer
          (gptel-context-files-mode)
          (gptel-plus-list-context-files-internal)
          ;; The large file should appear first in the buffer
          (goto-char (point-min))
          (let ((large-pos (save-excursion
                             (search-forward (file-name-nondirectory large) nil t)))
                (small-pos (save-excursion
                             (search-forward (file-name-nondirectory small) nil t))))
            (should large-pos)
            (should small-pos)
            (should (< large-pos small-pos))))
      (delete-file small)
      (delete-file large))))

(ert-deftest gptel-plus-test-list-context-files-internal-path-abbreviation ()
  "Home directory paths are abbreviated with ~/."
  (let* ((home-file (expand-file-name "~/test-gptel-context-file.txt"))
         (gptel-context (list (list home-file))))
    (unwind-protect
        (progn
          (with-temp-file home-file (insert "content"))
          (with-temp-buffer
            (gptel-context-files-mode)
            (gptel-plus-list-context-files-internal)
            (goto-char (point-min))
            ;; Should show ~/ abbreviation
            (should (search-forward "~/test-gptel-context-file.txt" nil t))
            ;; Should NOT show the full expanded path
            (goto-char (point-min))
            (should-not (search-forward (expand-file-name "~/") nil t))))
      (when (file-exists-p home-file)
        (delete-file home-file)))))

(ert-deftest gptel-plus-test-list-context-files-internal-text-properties ()
  "Each file entry gets gptel-context-file and gptel-flag text properties."
  (let* ((f (gptel-plus-test--make-temp-file "some content"))
         (gptel-context (list (list f))))
    (unwind-protect
        (with-temp-buffer
          (gptel-context-files-mode)
          (gptel-plus-list-context-files-internal)
          ;; Find the line with the file entry (after header)
          (goto-char (point-min))
          (search-forward "[ ]" nil t)
          (let ((line-start (line-beginning-position)))
            (should (equal (get-text-property line-start 'gptel-context-file) f))
            (should (null (get-text-property line-start 'gptel-flag)))))
      (delete-file f))))

(ert-deftest gptel-plus-test-list-context-files-internal-skips-buffers ()
  "Buffer entries in context are filtered out (only files listed)."
  (let ((buf (generate-new-buffer " *test-buf-ctx*"))
        (f (gptel-plus-test--make-temp-file "file content")))
    (unwind-protect
        (let ((gptel-context (list (list buf) (list f))))
          (with-temp-buffer
            (gptel-context-files-mode)
            (gptel-plus-list-context-files-internal)
            ;; Count [ ] markers - should be 1 (only the file)
            (goto-char (point-min))
            (let ((count 0))
              (while (search-forward "[ ]" nil t)
                (cl-incf count))
              (should (= count 1)))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-file f))))

;;;; Cost warning

(ert-deftest gptel-plus-test-confirm-costs-below-threshold ()
  "No prompt when cost is below threshold."
  (gptel-plus-test-with-model 3.0 15.0
    (let ((gptel-plus-cost-warning-threshold 1.0))
      (gptel-plus-test-with-temp-buffer "word"
        ;; Cost is ~$0.0015 which is well below $1.0
        ;; Should not prompt, should not error
        (gptel-plus-confirm-when-costs-high)))))

(ert-deftest gptel-plus-test-confirm-costs-disabled ()
  "No prompt when threshold is nil."
  (gptel-plus-test-with-model 3.0 15.0
    (let ((gptel-plus-cost-warning-threshold nil))
      (gptel-plus-test-with-temp-buffer "word"
        (gptel-plus-confirm-when-costs-high)))))

(ert-deftest gptel-plus-test-confirm-costs-high-accepted ()
  "User accepts high cost - no error."
  (gptel-plus-test-with-model 3.0 15.0
    (let ((gptel-plus-cost-warning-threshold 0.0))
      (gptel-plus-test-with-temp-buffer "word"
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
          (gptel-plus-confirm-when-costs-high))))))

(ert-deftest gptel-plus-test-confirm-costs-high-rejected-no-clear ()
  "User rejects high cost and declines clearing context - user-error raised."
  (gptel-plus-test-with-model 3.0 15.0
    (let ((gptel-plus-cost-warning-threshold 0.0))
      (gptel-plus-test-with-temp-buffer "word"
        (let ((call-count 0))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (_) (cl-incf call-count) nil)))
            (should-error (gptel-plus-confirm-when-costs-high)
                          :type 'user-error)))))))

(ert-deftest gptel-plus-test-confirm-costs-high-rejected-clear ()
  "User rejects cost but clears context - context removal called, then error."
  (gptel-plus-test-with-model 3.0 15.0
    (let ((gptel-plus-cost-warning-threshold 0.0)
          (cleared nil))
      (gptel-plus-test-with-temp-buffer "word"
        (let ((call-count 0))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (_)
                       (cl-incf call-count)
                       ;; First call (continue?) = no, second (clear?) = yes
                       (= call-count 2)))
                    ((symbol-function 'gptel-context-remove-all)
                     (lambda () (setq cleared t))))
            (should-error (gptel-plus-confirm-when-costs-high)
                          :type 'user-error)
            (should cleared)))))))

;;;; Unload function

(ert-deftest gptel-plus-test-unload-removes-advice ()
  "Unload function removes all advice and hooks."
  ;; Ensure advice is present before unloading
  (advice-add 'gptel-context-add-file :after #'gptel-plus-update-context-cost)
  (advice-add 'gptel-context-remove :after #'gptel-plus-update-context-cost)
  (advice-add 'gptel--set-with-scope :after #'gptel-plus--update-cost-on-model-change)
  (advice-add 'gptel-send :before #'gptel-plus-confirm-when-costs-high)
  (add-hook 'gptel-post-response-functions #'gptel-plus-calculate-exact-cost)
  (add-hook 'gptel-post-request-hook #'gptel-plus-prepare-cost-calculation)
  ;; Unload
  (should (null (gptel-plus-unload-function)))
  ;; Verify advice removed
  (should-not (advice-member-p #'gptel-plus-update-context-cost 'gptel-context-add-file))
  (should-not (advice-member-p #'gptel-plus-update-context-cost 'gptel-context-remove))
  (should-not (advice-member-p #'gptel-plus--update-cost-on-model-change 'gptel--set-with-scope))
  (should-not (advice-member-p #'gptel-plus-confirm-when-costs-high 'gptel-send))
  ;; Verify hooks removed
  (should-not (memq #'gptel-plus-calculate-exact-cost gptel-post-response-functions))
  (should-not (memq #'gptel-plus-prepare-cost-calculation gptel-post-request-hook))
  ;; Re-add so the package still works for remaining tests
  (advice-add 'gptel-context-add-file :after #'gptel-plus-update-context-cost)
  (advice-add 'gptel-context-remove :after #'gptel-plus-update-context-cost)
  (advice-add 'gptel--set-with-scope :after #'gptel-plus--update-cost-on-model-change)
  (advice-add 'gptel-send :before #'gptel-plus-confirm-when-costs-high)
  (add-hook 'gptel-post-response-functions #'gptel-plus-calculate-exact-cost 100)
  (add-hook 'gptel-post-request-hook #'gptel-plus-prepare-cost-calculation))

;;;; Constants

(ert-deftest gptel-plus-test-local-variables-list ()
  "Local variables list contains the expected gptel variables."
  (should (memq 'gptel-mode gptel-plus-local-variables))
  (should (memq 'gptel-model gptel-plus-local-variables))
  (should (memq 'gptel--backend-name gptel-plus-local-variables))
  (should (memq 'gptel--bounds gptel-plus-local-variables)))

(ert-deftest gptel-plus-test-org-properties-list ()
  "Org properties list contains the expected properties."
  (should (member "GPTEL_SYSTEM" gptel-plus-org-properties))
  (should (member "GPTEL_MODEL" gptel-plus-org-properties))
  (should (member "GPTEL_BACKEND" gptel-plus-org-properties)))

(provide 'gptel-plus-test)
;;; gptel-plus-test.el ends here
