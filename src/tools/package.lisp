;;;; src/tools/package.lisp
;;;; Define package for all tools

(defpackage :cl-tron-mcp/tools
  (:use :cl)
  (:import-from :cl-tron-mcp/config
                #:*version*)
  (:import-from :cl-tron-mcp/swank
                #:swank-connect
                #:swank-disconnect
                #:swank-status
                #:mcp-swank-eval
                #:mcp-swank-compile
                #:mcp-swank-threads
                #:mcp-swank-abort
                #:mcp-swank-backtrace
                #:mcp-swank-frame-locals
                #:mcp-swank-inspect
                #:mcp-swank-describe
                #:mcp-swank-completions
                #:mcp-swank-autodoc
                #:swank-step
                #:swank-next
                #:swank-out
                #:swank-continue
                #:swank-get-restarts
                #:swank-invoke-restart
                #:mcp-swank-set-breakpoint
                #:mcp-swank-remove-breakpoint
                #:mcp-swank-list-breakpoints
                #:mcp-swank-toggle-breakpoint)
  
  (:export
    #:handle-tools-list
    #:handle-tool-call
    #:define-simple-tool
    #:define-validated-tool
    #:register-tool-handler
    #:list-tool-descriptors
    #:repl-help

   ;; Validation API (src/tools/validation.lisp). The tests already import
   ;; these by name, and a tool defined outside this package -- the
   ;; installation tools are -- has to be able to call them, so they are
   ;; external rather than merely internal.
   #:validation-error
   #:validation-error-message
   #:validation-error-parameter
   #:validate-required
   #:validate-string
   #:validate-integer
   #:validate-boolean
   #:validate-choice
   #:validate-object-id
   #:validate-symbol-name
   #:validate-package-name

   ;; Tool-call execution support (src/tools/handlers-support.lisp;
   ;; cl-tron-mcp#2 "bug #8" -- relocated here, not shared/imported
   ;; from cl-tron-mcp/protocol; see that file's header comment)
   #:validate-string-param
   #:validate-list-param
   #:make-error-response
   #:cleanup-on-error
   #:timeout-error
   #:timeout-error-message
   #:*default-tool-timeout*
   #:*pending-requests*
   #:*request-lock*

   ;; Connection
   #:repl-connect
   #:repl-disconnect
   #:repl-connected-p
   #:repl-status
   #:repl-type

   #:*repl-connected*
   #:*repl-type*
   #:*repl-port*
   #:*repl-host*

   ;; Evaluation
   #:repl-eval
   #:repl-compile

   ;; Operations
   #:repl-threads
   #:repl-abort
   #:repl-backtrace
   #:repl-frame-locals
   #:repl-inspect
   #:repl-describe
   #:repl-completions
   #:repl-doc

   ;; Debugger
   #:repl-step
   #:repl-next
   #:repl-out
   #:repl-continue
   #:repl-get-restarts
   #:repl-invoke-restart

   ;; Breakpoints
   #:repl-set-breakpoint
   #:repl-remove-breakpoint
   #:repl-list-breakpoints
   #:repl-toggle-breakpoint))
