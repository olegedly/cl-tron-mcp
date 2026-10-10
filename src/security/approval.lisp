;;;; src/security/approval.lisp

(in-package :cl-tron-mcp/security)

(defparameter *approval-required-operations*
  '(:eval :compile-file :modify-running-code :terminate-thread
    :set-breakpoint :trace-function :modify-restarts :launch-process))

(defvar *pending-approvals* (make-hash-table :test 'equal))
(defvar *approval-lock* (bt:make-lock "approval"))
(defvar *approval-timeout* 300)

(defvar *approval-whitelist* (make-hash-table :test 'equal))
(defvar *whitelist-enabled* nil)
;; One-time use: when client sends approval/respond(approved: true), we add request_id here.
;; When tools/call is re-invoked with approval_request_id, we consume and run the tool.
(defvar *approved-request-ids* (make-hash-table :test 'equal))

;;; Atomic counter for unique request IDs
(defvar *approval-request-counter* 0
  "Atomic counter for generating unique approval request IDs.")

(defvar *approval-counter-lock* (bt:make-lock "approval-counter")
  "Lock for synchronizing access to *approval-request-counter*.")

(defstruct approval-request id operation details timestamp expires response actor approved-at)

(defun operation-requires-approval (operation)
  (member operation *approval-required-operations*))

(defun coerce-operation-key (operation)
  "Normalize OPERATION to a keyword regardless of whether it's a string or keyword."
  (etypecase operation
    (keyword operation)
    (string (intern (string-upcase operation) :keyword))))

(defun whitelist-add (operation pattern)
  (let ((op (coerce-operation-key operation)))
    (bt:with-lock-held (*approval-lock*)
      (push pattern (gethash op *approval-whitelist*))
      (list :operation op :pattern pattern))))

(defun whitelist-remove (operation pattern)
  (let ((op (coerce-operation-key operation)))
    (bt:with-lock-held (*approval-lock*)
      (let ((patterns (gethash op *approval-whitelist*)))
        (when patterns
          (setf (gethash op *approval-whitelist*)
                (remove pattern patterns :test #'equal))))
      (list :operation op :pattern pattern :removed t))))

(defun whitelist-clear (&optional operation)
  (bt:with-lock-held (*approval-lock*)
    (if operation
        (let ((op (coerce-operation-key operation)))
          (setf (gethash op *approval-whitelist*) nil))
        (clrhash *approval-whitelist*))
    (list :cleared operation)))

(defun whitelist-enable (&optional (enable t))
  (setq *whitelist-enabled* enable)
  (list :whitelist-enabled enable))

(defun whitelist-status ()
  (list :enabled *whitelist-enabled*))

(defun whitelist-match-p (pattern details)
  "Return t if DETAILS matches PATTERN.
PATTERN can be: t (wildcard), \"*\" (wildcard), a function (called with details),
or a string (substring match against details if string, or :tool key if plist)."
  (cond
    ((eq pattern t) t)
    ((functionp pattern) (funcall pattern details))
    ((stringp pattern)
     (when (string= pattern "*") (return-from whitelist-match-p t))
     (let ((target (cond
                     ((stringp details) details)
                     ((listp details) (or (getf details :tool)
                                          (getf details :operation-details)
                                          ""))
                     (t ""))))
       (search pattern (or target "") :test #'string-equal)))
    (t nil)))

(defun whitelist-check (operation details)
  "Return t if OPERATION with DETAILS is whitelisted (no user approval needed).
Checks stored patterns for the given operation."
  (when *whitelist-enabled*
    (bt:with-lock-held (*approval-lock*)
      (let ((patterns (gethash operation *approval-whitelist*)))
        (some (lambda (pattern) (whitelist-match-p pattern details))
              (or patterns '()))))))

;; Map MCP tool name (string) to approval operation keyword for whitelist lookup.
(defparameter *tool-name-to-operation*
  (let ((h (make-hash-table :test 'equal)))
    (dolist (pair '(("repl_eval" . :eval) ("repl_compile" . :compile-file)
                    ("swank_eval" . :eval) ("swank_compile" . :compile-file)
                    ("code_compile_string" . :compile-file) ("reload_system" . :modify-running-code)
                    ("profile_start" . :modify-running-code) ("profile_stop" . :modify-running-code)
                    ("trace_function" . :trace-function) ("trace_remove" . :trace-function)
                    ("breakpoint_set" . :set-breakpoint) ("repl_set_breakpoint" . :set-breakpoint)
                    ("swank_abort" . :terminate-thread) ("swank_interrupt" . :terminate-thread)
                    ("repl_abort" . :terminate-thread)
                    ("swank_launch" . :launch-process) ("swank_kill" . :launch-process)))
      (setf (gethash (car pair) h) (cdr pair)))
    h))

(defun tool-name-to-operation (tool-name)
  (gethash (string tool-name) *tool-name-to-operation*))

(defun whitelist-check-tool (tool-name arguments)
  "Return t if this tool/arguments are whitelisted (no user approval needed)."
  (let ((op (tool-name-to-operation tool-name)))
    (when op
      (whitelist-check op (list :tool tool-name :arguments arguments)))))

(defun generate-approval-request-id ()
  "Return a unique string ID for an approval request (JSON-RPC safe).
Uses atomic counter to ensure uniqueness even when called concurrently."
  (bt:with-lock-held (*approval-counter-lock*)
    (incf *approval-request-counter*)
    (format nil "~a-~a-~a"
            (get-unix-time)
            *approval-request-counter*
            (random 10000))))

(defun request-approval (operation details &key (timeout *approval-timeout*) (actor "unknown"))
  "Create an approval request. ACTOR identifies who/what is requesting approval."
  (let* ((req-id (generate-approval-request-id))
         (now (get-unix-time))
         (request (make-approval-request
                   :id req-id
                   :operation operation
                   :details details
                   :timestamp now
                   :expires (+ now timeout)
                   :actor actor)))
    (bt:with-lock-held (*approval-lock*)
      (setf (gethash req-id *pending-approvals*) request))
    request))

(defun approval-response (request-id &optional response &key (actor "user"))
  "Record user response for request-id. ACTOR identifies who approved/denied.
When response is :approved, add to *approved-request-ids* with timestamp for cleanup."
  (bt:with-lock-held (*approval-lock*)
    (let ((request (gethash request-id *pending-approvals*)))
      (unless request
        (error "Approval request ~a not found" request-id))
      (when response
        (setf (approval-request-response request) response)
        (setf (approval-request-approved-at request) (get-unix-time))
        (setf (approval-request-actor request) actor)
        (remhash request-id *pending-approvals*)
        (when (eq response :approved)
          (setf (gethash request-id *approved-request-ids*) (get-unix-time))))
      (approval-request-response request))))

(defun consume-approved-request-id (request-id)
  "If request-id was approved (one-time), remove it and return t. Otherwise return nil."
  (bt:with-lock-held (*approval-lock*)
    (when (gethash request-id *approved-request-ids*)
      (remhash request-id *approved-request-ids*)
      t)))

(defun cleanup-expired-approvals ()
  "Remove expired approval requests from *pending-approvals*."
  (bt:with-lock-held (*approval-lock*)
    (let ((now (get-unix-time))
          (expired-count 0))
      (maphash (lambda (req-id request)
                 (when (> now (approval-request-expires request))
                   (remhash req-id *pending-approvals*)
                   (incf expired-count)))
               *pending-approvals*)
      (when (> expired-count 0)
        (cl-tron-mcp/logging:log-info
         (format nil "Cleaned up ~d expired approval requests" expired-count)))
      expired-count)))

(defun cleanup-old-approved-requests (&optional (max-age 300))
  "Remove approved request IDs older than MAX-AGE seconds (default 5 minutes)."
  (bt:with-lock-held (*approval-lock*)
    (let ((now (get-unix-time))
          (expired-count 0))
      (maphash (lambda (req-id timestamp)
                 (when (> (- now timestamp) max-age)
                   (remhash req-id *approved-request-ids*)
                   (incf expired-count)))
               *approved-request-ids*)
      (when (> expired-count 0)
        (cl-tron-mcp/logging:log-info
         (format nil "Cleaned up ~d old approved request IDs" expired-count)))
      expired-count)))

(defun cleanup-all-approvals ()
  "Clean up expired pending approvals and old approved request IDs."
  (let ((expired (cleanup-expired-approvals))
        (old-approved (cleanup-old-approved-requests)))
    (list :expired-pending expired
          :expired-approved old-approved)))

(defun check-approval (operation details)
  "Return t if operation is approved (whitelisted). Server-enforced approval uses
   approval_required response and approval/respond instead of blocking here."
  (whitelist-check operation details))

(defun get-unix-time ()
  (local-time:timestamp-to-unix (local-time:now)))

;;; ---------------------------------------------------------------------------
;;; Standing approval from the environment.
;;;
;;; The approval flow is built for a client with a human behind it: a guarded
;;; tools/call returns approval_required with a request_id, the human answers the
;;; `approval/respond` JSON-RPC method, and the client re-invokes the tool with
;;; that id. `approval/respond` is a raw method, not a tool, so an MCP client
;;; whose only interface is tools/call -- an agent harness, for instance -- has no
;;; way to answer, and the guarded tools are unreachable for it. The whitelist is
;;; the reachable alternative, but it lives in memory only and defaults to empty
;;; and disabled, so it cannot survive a server restart on its own.
;;;
;;; *WHITELIST-ENV-VAR* is the durable form: the launcher sets it, START-SERVER
;;; applies it, and the approval gate stays untouched for anyone who does not set
;;; it. This grants standing approval; it does not weaken the gate for unset runs.

(defparameter *whitelist-env-var* "CL_TRON_MCP_WHITELIST"
  "Environment variable that pre-populates the approval whitelist at startup.

Unset (or blank) leaves the whitelist empty and disabled -- the gate behaves
exactly as before. Two accepted forms:

  CL_TRON_MCP_WHITELIST='*'          every guarded operation, pattern '*'
  CL_TRON_MCP_WHITELIST='all'        same thing, spelled out
  CL_TRON_MCP_WHITELIST='eval:*,compile-file:*,set-breakpoint:*'
                                     listed operations, each with a pattern

Entries are separated by commas or semicolons. An entry is OPERATION or
OPERATION:PATTERN, PATTERN defaulting to '*'. OPERATION is one of
*APPROVAL-REQUIRED-OPERATIONS*; PATTERN follows WHITELIST-MATCH-P, so '*' is a
wildcard and anything else is a case-insensitive substring match against the tool
name. Unknown operations are skipped rather than signalled: this runs during
startup, and a typo in an environment variable should not keep the server down.")

(defun parse-whitelist-spec (spec)
  "Parse a *WHITELIST-ENV-VAR* value into a list of (OPERATION . PATTERN), or the
keyword :all for the wildcard spellings \"*\" and \"all\".

Unknown operations and blank entries are dropped, so the result can be NIL even
for a non-blank SPEC."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) spec)))
    (if (member trimmed '("*" "all") :test #'string-equal)
        :all
        (loop for raw in (uiop:split-string trimmed :separator '(#\, #\;))
              for entry = (string-trim '(#\Space #\Tab) raw)
              when (plusp (length entry))
                append (let* ((colon (position #\: entry))
                              (name (if colon (subseq entry 0 colon) entry))
                              (pattern (if colon (subseq entry (1+ colon)) "*"))
                              (operation (coerce-operation-key name)))
                         (when (operation-requires-approval operation)
                           (list (cons operation pattern))))))))

(defun configure-whitelist-from-env ()
  "Apply *WHITELIST-ENV-VAR* to the whitelist. Returns a summary plist when the
whitelist was enabled, or NIL when the variable is unset, blank, or held nothing
this build recognizes -- in which case nothing is enabled and the gate is as
strict as it was."
  (let ((spec (uiop:getenv *whitelist-env-var*)))
    (when (and spec (plusp (length (string-trim '(#\Space #\Tab) spec))))
      (let ((parsed (parse-whitelist-spec spec)))
        (cond
          ((eq parsed :all)
           (dolist (operation *approval-required-operations*)
             (whitelist-add operation "*"))
           (whitelist-enable t)
           (list :enabled t :scope :all
                 :operations (copy-list *approval-required-operations*)))
          (parsed
           (dolist (pair parsed)
             (whitelist-add (car pair) (cdr pair)))
           (whitelist-enable t)
           (list :enabled t :scope :listed
                 :operations (mapcar #'car parsed)))
          (t nil))))))
