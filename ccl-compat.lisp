;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CCL-COMPAT
;;;; © Michał "phoe" Herda 2017
;;;; ccl-compat.lisp

(in-package #:ccl)

;;;; Compatibility functions

(defun neq (x y)
  (not (eq x y)))

;;;; PKG-ARG

(defun pkg-arg (thing &optional deleted-ok)
  (let* ((xthing (cond ((or (symbolp thing) (typep thing 'character))
                        (string thing)) ;; turn into string
                       ((typep thing 'string)
                        (ensure-simple-string thing))
                       (t
                        thing))))
    (let* ((typecode (typecode xthing))) ;; we most likely won't need this
      (declare (fixnum typecode)) ;; since it's some kind of internal CCL tag
      (cond ((= typecode target::subtag-package) ;; if thing is a package
             (if (or deleted-ok ;; are deleted packages okay?
                     (pkg.names xthing)) ;; get package name
                 xthing ;; return the package
                 (error "~S is a deleted package ." thing))) ;; error
            ((= typecode target::subtag-simple-base-string) ;; package name?
             (or (%find-pkg xthing) ;; find package with that name
                 (%kernel-restart $xnopkg xthing))) ;; or lose
            (t (report-bad-arg thing 'simple-string)))))) ;; error on badarg

;;;; NO-SUCH-PACKAGE

(define-condition no-such-package (package-error) ()
  (:report (lambda (c s)
             (format s "Package ~S was not found." (package-error-package c)))))

;;;; REPORT-CONDITION

(defgeneric report-condition (condition stream))

(defmethod report-condition ((c condition) stream)
  (princ (cond ((typep c 'error) "Error ")
               ((typep c 'warning) "Warning ")
               (t "Condition "))
         stream)
  ;;Here should dump all slots or something.  For now...
  (let ((*print-escape* t))
    (print-object c stream)))

;;;; DEFINITION-BASE-NAME

(defclass definition-type ()
  ((name :allocation :class :reader definition-type-name :initform t))
  (:documentation "Superclass of all definition types"))

(defgeneric definition-base-name (def-type def)
  ;; Note that a def can have multiple base names, but each one needs
  ;; a different def-type
  (:documentation "Return the name that, when the user asks for all
definitions of that name, this def should be included.  Typically this is a
symbol.  It's used as a key in an EQ hash table, so must return EQ values for
equivalent definitions. The default method returns the rightmost atom in name.")
  (:method ((dt definition-type) name)
    (do ()
        ((not (consp name)) name)
      (let ((x (last name)))
        (setq name (or (cdr x) (car x)))))))

;;;; DEFINITION-TYPE-INSTANCE

(defmethod definition-type-instance
    ((dt definition-type) &key (if-does-not-exist :error))
  (if (rassoc dt *definition-types* :test #'eq)
      dt
      (ecase if-does-not-exist
        ((nil) nil)
        ((:error) (error "~s is not a known definition-type" dt)))))

(defmethod definition-type-instance
    ((name symbol) &key (if-does-not-exist :error))
  (or (cdr (assq name *definition-types*))
      (ecase if-does-not-exist
        ((nil) nil)
        ((:error) (error "~s is not a known definition-type" name))
        ((:create) (auto-create-definition-type name)))))

(defmethod definition-type-instance
    ((class class) &key (if-does-not-exist :error))
  (definition-type-instance (class-prototype class)
                            :if-does-not-exist if-does-not-exist))

;;;; METHOD-DEF-PARAMETERS

(defun classp (x)
  (if (%standard-instance-p x)
      (< (the fixnum (instance.hash x)) max-class-ordinal)
      (and (typep x 'macptr) (foreign-classp x))))

(defmacro %standard-instance-p (i)
  `(eq (typecode ,i) ,(type-keyword-code :instance)))

(defun method-def-parameters (m)
  (when (typep m 'method-function)
    (setq m (%method-function-method m)))
  (if (typep m 'method)
      (values (method-name m)
              (method-qualifiers m)
              (method-specializers m))
      (let (name quals specs data last)
        (when (consp m)
          (when (eq (car m) :method) (setq m (cdr m)))
          ;; (name spec1 .. specn) or (name qual1 .. qualn (spec1 ... specn))
          (setq data (cdr m) last (last data))
          (when (null (cdr last))
            (setq last (car last))
            (if (and (listp last) (neq (car last) 'eql))
                (setq quals (butlast data) specs last)
                (setq specs data))
            (setq name (car m))
            (when (and (or (non-nil-symbol-p name) (setf-function-name-p name))
                       (every #'(lambda (q) (not (listp q))) quals)
                       (every #'(lambda (s)
                                  (or (non-nil-symbol-p s)
                                      (classp s)
                                      (and (consp s)
                                           (consp (cdr s))
                                           (null (cddr s))
                                           (eq (car s) 'eql))))
                              specs))
              (values name quals specs)))))))

;;;; NON-NIL-SYMBOLP

(defun non-nil-symbol-p (x)
  "Returns symbol if true"
  (if (symbolp x) x))

;;;; SETF-FUNCTION-NAME-P

(defun setf-function-name-p (thing)
  (and (consp thing)
       (consp (cdr thing))
       (null (cddr thing))
       (eq (car thing) 'setf)
       (symbolp (cadr thing))))

;;;; PARSE-MACRO-1
(defun parse-macro-1 (name arglist body &optional env)
  (parse-macro-internal name arglist body env nil))

(defun parse-macro-internal (name arglist body env default-initial-value)
  (unless (verify-lambda-list arglist t t t)
    (error "Invalid lambda list ~s" arglist))
  (multiple-value-bind (lambda-list whole environment)
      (normalize-lambda-list arglist t t)
    (multiple-value-bind (body local-decs doc)
        (parse-body body env t)
      (let ((whole-var (gensym "WHOLE"))
            (env-var (gensym "ENVIRONMENT")))
        (multiple-value-bind (bindings binding-decls)
            (%destructure-lambda-list
             lambda-list whole-var nil nil
             :cdr-p t
             :whole-p nil
             :use-whole-var t
             :default-initial-value default-initial-value)
          (when environment
            (setq bindings (nconc bindings (list `(,environment ,env-var)))))
          (when whole
            (setq bindings (nconc bindings (list `(,whole ,whole-var)))))
          (values
           `(lambda (,whole-var ,env-var)
              (declare (ignorable ,whole-var ,env-var))
              (block ,name
                (let* ,(nreverse bindings)
                  ,@(when binding-decls `((declare ,@binding-decls)))
                  ,@local-decs
                  ,@body)))
           doc))))))

;;;; RECORD-ARGLIST

(defun record-arglist (name args)
  "Used by defmacro & defgeneric"
  (when (or *save-arglist-info* *save-local-symbols*)
    (setf (gethash name %lambda-lists%) args)))

;;;; CHEAP-EVAL-IN-ENVIRONMENT

(defun cheap-eval-in-environment (form env)
  ;; ENV is not used anywhere - this is a simple EVAL call
  (declare (ignore env))
  (eval form))

;;;; READ-RECORDING-SOURCE

(defun read-recording-source (stream &key eofval file-name start-offset
                                       map save-source-text)
  ;; We are not recording source - this is a simple READ call
  (declare (ignore map file-name start-offset save-source-text))
  (values (read stream nil eofval nil) nil))

;;;; *LOADING-TOPLEVEL-LOCATION*

(defparameter *loading-toplevel-location* nil)

;;;; NX-SOURCE-NOTE-MAP

(defvar *nx-source-note-map* nil)

;;;; BLOCK-LIKE

(defun block-like (xp list &rest args)
  (declare (ignore args))
  (funcall (formatter "~:<~1I~^~W~^ ~@_~W~^~@{ ~_~W~^~}~:>") xp list))

;;;; PROGN-PRINT

(defun progn-print (xp list)
  (funcall (formatter "~:<~1I~@{~W~^ ~_~}~:>") xp list))

;;;; *SHOW-CONDITION-CONTEXT*

(defvar *show-condition-context* t
  "The type of conditions which should include the execution context as part
of their error-output message.
   E.g. value of 'error will prevent warnings from including the calling
function and process in the warning message")
