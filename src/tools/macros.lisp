;;;; src/tools/macros.lisp
;;;; Tool registration macros for reducing boilerplate

(in-package :cl-tron-mcp/tools)

(defmacro define-validated-tool (name description &key input-schema output-schema requires-approval documentation-uri validation body)
  "Define a tool with validation wrapper.
   VALIDATION is a list of validation forms.
   BODY is the actual implementation.

   Lambda parameters are derived from BOTH input-schema keys AND validation forms,
   so that body can reference any declared input parameter even if it has no validation.

   Both derivations go through NORMALIZE-ARGUMENT-KEY -- the function the wire
   path itself applies to an incoming argument name -- rather than upcasing the
   name here. That is the point: a served property name and a handler
   parameter are then the same thing by construction, so `:|symbolName|' in the
   schema and `symbol_name' in the body are two spellings of one keyword (the
   one named SYMBOL_NAME), and neither can drift from the other unnoticed. A
   validation that names a parameter the schema does not advertise is refused
   at macroexpansion, because a caller could never supply it and the validation
   would fail on every call."
  (flet ((parameter (name)
           ;; Keywords may not appear in a lambda list -- they are constants --
           ;; so the keyword NORMALIZE-ARGUMENT-KEY answers with is re-interned
           ;; in this package. &key matches by name, not by identity, so the
           ;; handler still binds the keyword the wire produces.
           (intern (symbol-name (normalize-argument-key name)))))
    (let* ((schema-params (when input-schema
                            (loop for (k v) on (eval input-schema) by #'cddr
                                  collect (parameter (schema-property-name k)))))
           (validation-params (when validation
                                (loop for (func param . args) in validation
                                      collect (parameter param))))
           (orphans (set-difference validation-params schema-params :test #'eq)))
      (when orphans
        (error "~a: validation names ~{~a~^, ~}, which :input-schema does not ~
                advertise (~{~a~^, ~}). The body could never receive ~
                ~[it~:;them~]; add the property to the schema or drop the ~
                validation."
               name orphans schema-params (length orphans)))
      (let ((params (remove-duplicates (append schema-params validation-params)
                                       :test #'eq)))
        `(progn
           (register-tool ,name ,description
                          :input-schema ,input-schema
                          :output-schema ,output-schema
                          :requires-approval ,requires-approval
                          :documentation-uri ,documentation-uri)
           (register-tool-handler ,name
                                  (lambda (&key ,@params)
                                    (handler-case
                                        (progn
                                          ,@(loop for (func param . args) in validation
                                                  collect `(,func ,(string-downcase param) ,@args))
                                          ,body)
                                      (validation-error (e)
                                        (list :error t
                                              :message (format nil "Validation error: ~a" (validation-error-message e))
                                              :parameter (validation-error-parameter e)))))))))))

(defmacro define-simple-tool (name description &key input-schema output-schema requires-approval documentation-uri function)
  "Define a tool that directly calls a function without validation."
  `(progn
     (register-tool ,name ,description
                    :input-schema ,input-schema
                    :output-schema ,output-schema
                    :requires-approval ,requires-approval
                    :documentation-uri ,documentation-uri)
     (register-tool-handler ,name (function ,function))))
