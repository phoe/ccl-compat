;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CCL-COMPAT
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:ccl-compat
  (:use #:cl)
  #-:lispworks (:nicknames #:ccl)
  (:export
   #:pkg-arg ;; function, DONE, edited
   #:no-such-package ;; condition, DONE, edited
   #:report-condition ;; generic function, DONE, portable
   #:definition-base-name ;; generic, DONE, turned WHILE into DO
   #:definition-type-instance ;; generic, DONE, stubbed
   #:method-def-parameters ;; function, DONE, edited/ported
   #:non-nil-symbolp ;; function, DONE, portable
   #:setf-function-name-p ;; function, DONE, portable after edit
   #:parse-macro-1 ;; function, DONE, pulled a lot of CCL dependencies
   #:record-arglist ;; function, DONE, stubbed out
   #:cheap-eval-in-environment ;; function, DONE, stubbed with CL:EVAL
   #:read-recording-source ;; function, DONE, stubbed with CL:READ
   #:*loading-toplevel-location* ;; variable, DONE, not implemented
   #:*nx-source-note-map* ;; variable, DONE, not implemented
   #:block-like ;; function, DONE, portable
   #:progn-print ;; function, DONE, portable
   #:*print-right-margin* ;; variable, DONE, standard
   #:*show-condition-context* ;; variable, DONE, not implemented
   #:assq ;; function, DONE, copied
   #:non-nil-symbolp ;; function, DONE, mapped to non-nil-symbol-p
   #:whitespacep ;; function, DONE, reimplemented
   #:proclaimed-special-p ;; function, DONE, stubbed as CONSTANTLY T
   #:get-type-predicate ;; function, DONE, stubbed as CONSTANTLY T
   #:require-type ;; function, DONE, reimplemented with TYPEP
   #:*loading-file-source-file* ;; variable, DONE, not implemented
   #:neq ;; function, DONE, = NOT EQ
   #:*save-source-locations* ;; variable, DONE, not implemented
   #:memq ;; function, DONE, ported
   #:nfunction ;; macro, DONE, stubbed using ALEXANDRIA:NAMED-LAMBDA
   #:record-source-file ;; function, DONE, stubbed
   ))
