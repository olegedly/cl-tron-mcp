;;;; tests/tool-schema-test.lisp
;;;; A tool's advertised schema and its handler must name the same parameters.

(in-package :cl-tron-mcp/tests)

#+sbcl (eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-introspect))

;;; Why this file exists.
;;;
;;; A tool's schema is a promise about what its handler can bind. Nothing used
;;; to compare the two, and they drifted: the schema said :symbolName, the
;;; reader destroyed its case, the served property became "symbolname", and
;;; NORMALIZE-ARGUMENT-KEY turned that into a keyword named SYMBOLNAME -- while
;;; the lambda list, written from the validation, wanted SYMBOL_NAME. Twenty-one
;;; tools advertised a property they could never receive and answered every call
;;; with "Required parameter is missing". Each side looked right on its own, so
;;; only a caller could notice.

(defun schema-property-names (schema)
  "The property names a normalised input SCHEMA serves to a client, sorted."
  (let ((properties (and (hash-table-p schema) (gethash "properties" schema))))
    (and properties
         (sort (loop for k being the hash-keys of properties collect k)
               #'string<))))

#+sbcl
(defun handler-key-parameters (handler)
  "The &key parameters HANDLER's lambda list takes, without defaults or
supplied-p variables."
  (let ((lambda-list (and handler (sb-introspect:function-lambda-list handler))))
    (when (and lambda-list (member '&key lambda-list))
      (loop with tail = (cdr (member '&key lambda-list))
            with keys = '()
            while tail
            for item = (pop tail)
            do (cond ((eq item '&allow-other-keys) (setf tail nil))
                     ((consp item) (push (first item) keys))
                     (t (push item keys)))
            finally (return (nreverse keys))))))

(defun advertised-p (name descriptor)
  "T if the tool's served schema advertises the string NAME."
  (member name
          (schema-property-names (gethash :|inputSchema| descriptor))
          :test #'string=))

(deftest test-authored-case-survives-the-reader
    (testing "a pipe-escaped key is served as written; an unescaped one downcased"
             (ok (equal (schema-property-names
                         (cl-tron-mcp/tools::normalize-tool-schema
                          (list :|symbolName| "string")))
                        '("symbolName")))
             (ok (equal (schema-property-names
                         (cl-tron-mcp/tools::normalize-tool-schema
                          (list :symbolname "string")))
                        '("symbolname")))))

(deftest test-served-name-binds-the-handler-keyword
    (testing "a camelCase property and a snake_case parameter are one keyword"
             (ok (string= (symbol-name (cl-tron-mcp/tools::normalize-argument-key
                                        "symbolName"))
                          "SYMBOL_NAME"))
             (ok (eq (cl-tron-mcp/tools::normalize-argument-key "symbolName")
                     (cl-tron-mcp/tools::normalize-argument-key "symbol_name")))))

(deftest test-xref-tools-advertise-their-parameter
    (testing "who_calls and friends serve the symbol name they bind"
             ;; The concrete regression: these five answered every call with
             ;; "Required parameter is missing symbol_name".
             (dolist (name '("who_calls" "who_references" "who_binds"
                             "who_sets" "list_callees"))
               (let ((entry (gethash name cl-tron-mcp/tools::*tool-registry*)))
                 (ok entry)
                 (ok (advertised-p "symbolName"
                                   (cl-tron-mcp/tools::tool-entry-descriptor entry)))))))

#+sbcl
(deftest test-every-advertised-property-reaches-a-handler-keyword
    (testing "a property a tool serves binds a &key its handler takes"
             ;; The other direction of the same seam: skill_recommend used to
             ;; advertise "agent_type" while its handler took one positional
             ;; argument, so every call died as an arity error.
             (let (offenders)
               (maphash
                (lambda (name entry)
                  (declare (ignore name))
                  (let* ((descriptor (cl-tron-mcp/tools::tool-entry-descriptor entry))
                         (handler (cl-tron-mcp/tools::tool-entry-handler entry))
                         (keys (mapcar #'symbol-name (handler-key-parameters handler))))
                    (dolist (property (schema-property-names
                                       (gethash :|inputSchema| descriptor)))
                      (unless (member (symbol-name
                                       (cl-tron-mcp/tools::normalize-argument-key property))
                                      keys :test #'string=)
                        (push (list (gethash :|name| descriptor) property)
                              offenders)))))
                cl-tron-mcp/tools::*tool-registry*)
               (ok (null offenders)))))

(deftest test-a-validation-the-schema-cannot-supply-is-refused
    (testing "define-validated-tool refuses a tool whose validation names a
              parameter no caller could pass"
             (ok (handler-case
                     (macroexpand-1
                      '(cl-tron-mcp/tools:define-validated-tool "nobody"
                          "a tool that could never have worked"
                        :input-schema (list :|symbolName| "string")
                        :validation ((validate-symbol-name "symbol_name" symbol_name
                                                           :required t))
                        :body symbol_name))
                   (error () t)))))
