;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CCL-COMPAT
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:ccl
  (:use #:cl)
  (:export ;; just export them for now, implement later
   #:pkg-arg #:no-such-package
   #:report-condition
   #:definition-base-name #:definition-type-instance
   #:method-def-parameters
   #:non-nil-symbolp #:setf-function-name-p
   #:parse-macro-1
   #:record-arglist
   #:cheap-eval-in-environment
   #:read-recording-source #:*loading-toplevel-location*
   #:*nx-source-note-map*
   #:block-like #:progn-print #:*print-right-margin*
   #:*show-condition-context*))
