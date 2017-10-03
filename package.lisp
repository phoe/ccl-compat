;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CCL-COMPAT
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:ccl
  (:use #:cl)
  (:export
   #:pkg-arg ;; function, TODO, documented
   #:no-such-package ;; condition, DONE, edited
   #:report-condition ;; generic function, DONE, portable
   #:definition-base-name ;; generic, DONE, turned WHILE into DO
   #:definition-type-instance ;; generic, TODO
   #:method-def-parameters ;; function, TODO
   #:non-nil-symbolp ;; function, DONE, portable
   #:setf-function-name-p ;; function, DONE, portable after edit
   #:parse-macro-1 ;; function, TODO
   #:record-arglist ;; function, TODO
   #:cheap-eval-in-environment ;; function, TODO, complex
   #:read-recording-source ;; function, TODO, complex
   #:*loading-toplevel-location* ;; variable, TODO, needs to be implemented?
   #:*nx-source-note-map* ;; variable, TODO, needs to be implemented?, hashtable
   #:block-like ;; function, DONE, portable
   #:progn-print ;; function, DONE, portable
   #:*print-right-margin* ;; variable, DONE, standard
   #:*show-condition-context* ;; variable, TODO, needs to be implemented?
   ))
