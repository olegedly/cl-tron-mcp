;;;; src/installation/package.lisp

(defpackage :cl-tron-mcp/installation
  (:use :cl)
  (:import-from :cl-tron-mcp/tools
                #:validate-string)
  (:export
    #:get-skill-installation-guide
    #:skill-store-installation-guide
    #:discover-skills
    #:recommend-skill-handler))
