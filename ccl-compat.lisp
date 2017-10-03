;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CCL-COMPAT
;;;; © Michał "phoe" Herda 2017
;;;; ccl-compat.lisp

(in-package #:ccl)

;;;; PKG-ARG

(defun pkg-arg (thing &optional deleted-ok)
  (let* ((xthing (cond ((or (symbolp thing) (typep thing 'character))
                        (string thing))
                       ((typep thing 'string)
                        (ensure-simple-string thing))
                       (t
                        thing))))
    (let* ((typecode (typecode xthing)))
      (declare (fixnum typecode))
      (cond ((= typecode target::subtag-package)
             (if (or deleted-ok (pkg.names xthing))
                 xthing
                 (error "~S is a deleted package ." thing)))
            ((= typecode target::subtag-simple-base-string)
             (or (%find-pkg xthing)
                 (%kernel-restart $xnopkg xthing)))
            (t (report-bad-arg thing 'simple-string))))))

;;;; NO-SUCH-PACKAGE

(define-condition no-such-package (package-error) ()
  (:report (lambda (c s)
             (format s (%rsc-string $xnopkg) (package-error-package c)))))

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
    (while (consp name)
           (let ((x (last name)))
             (setq name (or (cdr x) (car x)))))
    name))

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

;;;; NON-NIL-SYMBOLP

(defun non-nil-symbolp (x)
  "Returns symbol if true"
  (if (symbolp x) x))

;;;; METHOD-DEF-PARAMETERS

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

(defun cheap-eval-in-environment (form env &aux sym)
  ;; Allow ADVICE, TRACE to have effects on self-calls.
  (declare (notinline cheap-eval-in-environment))
  ;; records source locations if *nx-source-note-map* is bound by caller
  (setq *loading-toplevel-location*
        (or (nx-source-note form) *loading-toplevel-location*))
  (flet ((progn-in-env (body&decls parse-env base-env)
           (multiple-value-bind (body decls) (parse-body body&decls parse-env)
             (setq base-env
                   (augment-environment
                    base-env
                    :declare (decl-specs-from-declarations decls)))
             (loop with default-location = *loading-toplevel-location*
                   while (cdr body) as form = (pop body)
                   do (cheap-eval-in-environment form base-env)
                   do (setq *loading-toplevel-location* default-location))
             (cheap-eval-in-environment (car body) base-env))))
    (if form
        (cond
          ((symbolp form)
           (multiple-value-bind (expansion win)
               (cheap-eval-macroexpand-1 form env)
             (if win
                 (cheap-eval-in-environment expansion env)
                 (let* ((defenv (definition-environment env))
                        (constant
                          (if defenv (assq form (defenv.constants defenv))))
                        (constval (%cdr constant)))
                   (if constant
                       (if (neq (%unbound-marker-8) constval)
                           constval
                           (error "Can't determine value of constant symbol ~s"
                                  form))
                       (if (constant-symbol-p form)
                           (%sym-global-value form)
                           (symbol-value form)))))))
          ((atom form) form)
          ((eq (setq sym (%car form)) 'quote)
           (verify-arg-count form 1 1)
           (%cadr form))
          ((eq sym 'function)
           (verify-arg-count form 1 1)
           (cond ((symbolp (setq sym (%cadr form)))
                  (multiple-value-bind (kind local-p)
                      (function-information sym env)
                    (if (and local-p (eq kind :macro))
                        (error "~s can't be used to reference lexically ~
defined macro ~S" 'function sym)))
                  (%function sym))
                 ((setf-function-name-p sym)
                  (multiple-value-bind (kind local-p)
                      (function-information sym env)
                    (if (and local-p (eq kind :macro))
                        (error "~s can't be used to reference lexically ~
defined macro ~S" 'function sym)))
                  (%function (setf-function-name (%cadr sym))))
                 (t (cheap-eval-function nil sym env))))
          ((eq sym 'nfunction)
           (verify-arg-count form 2 2)
           (cheap-eval-function (%cadr form) (%caddr form) env))
          ((eq sym 'progn) (progn-in-env (%cdr form) env env))
          ((eq sym 'setq)
           (if (not (%ilogbitp 0 (list-length form)))
               (verify-arg-count form 0 0)) ;Invoke a "Too many args" error.
           (let* ((sym nil)
                  (val nil)
                  (original form))
             (while (setq form (%cdr form))
                    (setq sym (require-type (pop form) 'symbol))
                    (multiple-value-bind (expansion expanded)
                        (cheap-eval-macroexpand-1 sym env)
                      (if expanded
                          (setq val (cheap-eval-in-environment
                                     (cheap-eval-transform
                                      original `(setf ,expansion ,(%car form)))
                                     env))
                          (set sym (setq val (cheap-eval-in-environment
                                              (%car form) env))))))
             val))
          ((eq sym 'eval-when)
           (destructuring-bind (when . body) (%cdr form)
             (when (or (memq 'eval when) (memq :execute when))
               (progn-in-env body env env))))
          ((eq sym 'if)
           (destructuring-bind (test true &optional false) (%cdr form)
             (setq test (let ((*loading-toplevel-location*
                                *loading-toplevel-location*))
                          (cheap-eval-in-environment test env)))
             (cheap-eval-in-environment (if test true false) env)))
          ((eq sym 'locally) (progn-in-env (%cdr form) env env))
          ((and (symbolp sym)
                (compiler-special-form-p sym)
                (not (functionp (fboundp sym))))
           (if (eq sym 'unwind-protect)
               (destructuring-bind (protected-form . cleanup-forms) (cdr form)
                 (unwind-protect
                      (let ((*loading-toplevel-location*
                              *loading-toplevel-location*))
                        (cheap-eval-in-environment protected-form env))
                   (progn-in-env cleanup-forms env env)))
               (let ((fn (cheap-eval-function
                          nil
                          (cheap-eval-transform
                           form `(lambda () (progn ,form))) env)))
                 (funcall fn))))
          ((and (symbolp sym) (macro-function sym env))
           (cheap-eval-in-environment (cheap-eval-macroexpand-1 form env) env))
          ((or (symbolp sym)
               (and (consp sym) (eq (%car sym) 'lambda)))
           (let ((args nil) (form-location *loading-toplevel-location*))
             (dolist (elt (%cdr form))
               (push (cheap-eval-in-environment elt env) args)
               (setq *loading-toplevel-location* form-location))
             (apply #'call-check-regs (if (symbolp sym)
                                          sym (cheap-eval-function nil sym env))
                    (nreverse args))))
          (t
           (signal-simple-condition 'simple-program-error
                                    "Car of ~S is not a function name or ~
lambda-expression." form))))))

;;;; READ-RECORDING-SOURCE

(defun read-recording-source (stream &key eofval file-name start-offset
                                       map save-source-text)
  "Read a top-level form, perhaps recording source locations.
If MAP is NIL, just reads a form as if by READ.
If MAP is non-NIL, returns a second value of a source-note object describing
the form.
In addition, if MAP is a hash table, it gets filled with source-note's for all
non-atomic nested subforms."
  (when (null start-offset) (setq start-offset 0))
  (typecase map
    (null (values (read-internal stream nil eofval nil) nil))
    (hash-table
     (let* ((stream (recording-input-stream stream))
            (recording (list stream map file-name start-offset))
            (*recording-source-streams* (cons recording
                                              *recording-source-streams*)))
       (declare (dynamic-extent recording *recording-source-streams*))
       (multiple-value-bind (form source-note)
           (read-internal stream nil eofval nil)
         (when (and source-note (not (eq form eofval)))
           (assert (null (source-note.source source-note)))
           (when save-source-text
             (setf (source-note.source source-note)
                   (fetch-octets-from-stream
                    stream
                    (- (source-note-start-pos source-note)
                       start-offset)
                    (- (source-note-end-pos source-note)
                       start-offset)))))
         (values form source-note))))
    (T ;; not clear if this is ever useful
     (let* ((start-pos (stream-position stream))
            (form (read-internal stream nil eofval nil))
            (end-pos (and start-pos (neq form eofval) (stream-position stream)))
            (source-note
              (and end-pos
                   (make-source-note :filename file-name
                                     :start-pos (+ start-offset start-pos)
                                     :end-pos (+ start-offset end-pos)))))
       (when (and source-note save-source-text)
         (setf (source-note.source source-note)
               (fetch-octets-from-stream stream start-pos end-pos)))
       (values form source-note)))))

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
