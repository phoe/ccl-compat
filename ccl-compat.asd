;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CCL-COMPAT
;;;; © Michał "phoe" Herda 2017
;;;; ccl-compat.asd

(asdf:defsystem #:ccl-compat
  :description "Clozure CL compatibility module"
  :author "Michał \"phoe\" Herda <phoe@teknik.io>"
  :license "LLGPL"
  :serial t
  :depends-on
  #+ccl ()
  #-ccl (:closer-mop
         :alexandria
         :bordeaux-threads)
  :components
  #+ccl ()
  #-ccl ((:file "package")
         (:file "ccl-compat")))
