;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CCL-COMPAT
;;;; © Michał "phoe" Herda 2017
;;;; ccl-compat.lisp

(in-package #:ccl-compat)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DONE

;;;; PKG-ARG

(defun pkg-arg (thing &optional deleted-ok)
  (let* ((xthing (typecase thing
                   ((or symbol character) (string thing))
                   (string (coerce thing 'simple-string))
                   (t thing))))
    (cond ((packagep xthing)
           (if (or deleted-ok (package-name xthing))
             xthing
             (error "~S is a deleted package." thing)))
          ((stringp xthing)
           (or (find-package xthing)
               (error "There is no package named ~S." xthing)))
          (t (error "Cannot find package for datum: ~S" thing)))))

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

(defun definition-base-name (def-type def)
  (if (eq 'package def-type)
    (intern def :keyword)
    def))

;;;; DEFINITION-TYPE-INSTANCE

(defun definition-type-instance (dt &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  dt)

;;;; METHOD-DEF-PARAMETERS

(defun method-name (m)
  (c2mop:generic-function-name (c2mop:method-generic-function m)))

(defun method-def-parameters (m)
  (if (typep m 'method)
    (values (method-name m)
            (method-qualifiers m)
            (c2mop:method-specializers m))
    (let (name quals specs data last)
      (when (consp m)
        (when (eq (car m) :method) (setq m (cdr m)))
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
                                    (typep s 'class)
                                    (and (consp s)
                                         (consp (cdr s))
                                         (null (cddr s))
                                         (eq (car s) 'eql))))
                            specs))
            (values name quals specs)))))))

;;;; NON-NIL-SYMBOLP

(defun non-nil-symbolp (x)
  (non-nil-symbol-p x))

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

(defun %destructure-lambda-list
    (lambda-list wholeform  lets decls
     &key cdr-p (whole-p t) use-whole-var default-initial-value)
  (unless (and (listp lambda-list)
               (verify-lambda-list lambda-list t whole-p))
    (error "Invalid lambda list: ~s" lambda-list))
  (multiple-value-bind (normalized whole) (normalize-lambda-list
                                           lambda-list whole-p)
    (let* ((argstate :required)
           (allow-other-keys nil)
           (rest-arg-name nil)
           (w (if use-whole-var wholeform (or whole (gensym "WHOLE"))))
           (argptr (gensym "ARGS"))
           (has-&key nil)
           (keywords ())
           (first-keyword-init ())
           (restp nil))
      (labels ((simple-var (var &optional (initform `,default-initial-value))
                 (let* ((binding `(,var ,initform)))
                   (push  binding lets)
                   binding))
               (structured-var (context sub-lambda-list initform)
                 (let* ((v (gensym (string context))))
                   (simple-var v initform)
                   (multiple-value-setq (lets decls)
                     (%destructure-lambda-list
                      sub-lambda-list
                      v
                      lets
                      decls
                      :default-initial-value default-initial-value
                      ))
                   v)))
        (unless use-whole-var
          (if (atom w)
            (simple-var w wholeform)
            (progn
              (setq w (structured-var "WHOLE" w
                                      (if cdr-p `(cdr ,wholeform) wholeform))
                    cdr-p nil))))
        (multiple-value-bind (min max) (lambda-list-bounds normalized)
          (simple-var argptr `(prepare-to-destructure
                               ,@(if cdr-p `((cdr ,w)) `(,w))
                               ',lambda-list ,min ,max))
          (push `(ignorable ,argptr) decls)
          (when max
            (push `(list ,argptr) decls))
          (do* ((tail normalized (cdr tail)))
               ((null tail)
                (if has-&key
                  (let* ((key-check-form `(check-keywords
                                           ',(nreverse keywords)
                                           ,rest-arg-name ,allow-other-keys)))
                    (if first-keyword-init
                      (rplaca (cdr first-keyword-init)
                              `(progn
                                 ,key-check-form
                                 ,(cadr first-keyword-init)))
                      (let* ((check-var (gensym "CHECK")))
                        (push `(ignorable ,check-var) decls)
                        (simple-var check-var key-check-form)))))
                (values lets decls))
            (let* ((var (car tail)))
              (cond ((or (eq var '&rest) (eq var '&body))
                     (let* ((r (cadr tail))
                            (init argptr))
                       (if (listp r)
                         (setq rest-arg-name
                               (structured-var "REST" r init))
                         (progn
                           (setq rest-arg-name (gensym "REST"))
                           (simple-var rest-arg-name init)
                           (simple-var r rest-arg-name ))))
                     (setq restp t)
                     (setq tail (cdr tail)))
                    ((eq var '&optional) (setq argstate :optional))
                    ((eq var '&key)
                     (setq argstate :key)
                     (setq has-&key t)
                     (unless restp
                       (setq restp t
                             rest-arg-name (gensym "KEYS"))
                       (push `(ignorable ,rest-arg-name) decls)
                       (simple-var rest-arg-name
                                   argptr)))
                    ((eq var '&allow-other-keys)
                     (setq allow-other-keys t))
                    ((eq var '&aux)
                     (setq argstate :aux))
                    ((listp var)
                     (case argstate
                       (:required
                        (structured-var "REQ" var `(pop ,argptr)))
                       (:optional
                        (let* ((variable (car var))
                               (initform (if (cdr var)
                                           (cadr var)
                                           `,default-initial-value))
                               (anon-spvar (gensym "OPT-SUPPLIED-P"))
                               (spvar (if (cddr var)
                                        (caddr var)))
                               (varinit `(if ,anon-spvar
                                           (pop ,argptr)
                                           ,initform)))
                          (simple-var anon-spvar
                                      `(not (null  ,argptr)))
                          (if (listp variable)
                            (structured-var "OPT" variable varinit)
                            (simple-var variable varinit))
                          (if spvar
                            (simple-var spvar anon-spvar))))
                       (:key
                        (let* ((explicit-key (consp (car var)))
                               (variable (if explicit-key
                                           (cadar var)
                                           (car var)))
                               (keyword (if explicit-key
                                          (caar var)
                                          (make-keyword variable)))
                               (initform (if (cdr var)
                                           (cadr var)
                                           `,default-initial-value))
                               (anon-spvar (gensym "KEY-SUPPLIED-P"))
                               (spvar (if (cddr var)
                                        (caddr var))))
                          (push keyword keywords)
                          (let* ((sp-init (simple-var anon-spvar
                                                      `(%keyword-present-p
                                                        ,rest-arg-name
                                                        ',keyword)))
                                 (var-init `(if ,anon-spvar
                                              (getf ,rest-arg-name ',keyword)
                                              ,initform)))
                            (unless first-keyword-init
                              (setq first-keyword-init sp-init))
                            (if (listp variable)
                              (structured-var "KEY" variable var-init)
                              (simple-var variable var-init))
                            (if spvar
                              (simple-var spvar anon-spvar)))))
                       (:aux
                        (simple-var (car var) (cadr var)))
                       (t (error "NYI: ~s" argstate))))
                    ((symbolp var)
                     (case argstate
                       (:required
                        (simple-var var `(pop ,argptr)))
                       (:optional
                        (simple-var var `(if ,argptr
                                           (pop ,argptr)
                                           ',default-initial-value)))
                       (:key
                        (let* ((keyword (make-keyword var)))
                          (push keyword keywords)
                          (let* ((init
                                   (simple-var
                                    var
                                    `(getf ,rest-arg-name
                                           ',keyword
                                           ,@(if default-initial-value
                                               `(',default-initial-value))))))
                            (unless first-keyword-init
                              (setq first-keyword-init init)))))
                       (:aux
                        (simple-var var))))))))))))

(defun make-keyword (name)
  (if (and (symbolp name) (eq (symbol-package name) (find-package :keyword)))
    name
    (values (intern (string name) :keyword))))

(defun lambda-list-bounds (lambda-list)
  (let* ((state :required)
         (min 0)
         (max 0))
    (do* ((lambda-list lambda-list (cdr lambda-list)))
         ((null lambda-list) (values min max))
      (case (car lambda-list)
        ((&rest &key &body) (return (values min nil)))
        (&aux (return (values min max)))
        (&optional (setq state :optional))
        (t (ecase state
             (:required (incf min) (incf max))
             (:optional (incf max))))))))

(defun parse-body (body env &optional (doc-string-allowed t)
                   &aux decls doc (tail body) form)
  (declare (ignore env))
  (loop
    (if (endp tail) (return))
    (if (and (stringp (setq form (car tail))) (cdr tail))
      (if doc-string-allowed
        (setq doc form)
        (return))
      (if (not (and (consp form) (symbolp (car form))))
        (return)
        (if (eq (car form) 'declare)
          (push form decls)
          (return))))
    (setq tail (cdr tail)))
  (return-from parse-body (values tail (nreverse decls) doc)))

(defun verify-lambda-list (l &optional destructure-p whole-p env-p)
  (let* ((the-keys lambda-list-keywords)
         opttail
         resttail
         keytail
         allowothertail
         auxtail
         safecopy
         whole
         m
         n
         req
         sym
         (*structured-lambda-list* nil))
    (prog ()
       (unless (listp l) (go LOSE))
       (multiple-value-setq (safecopy whole)
         (normalize-lambda-list l whole-p env-p))
       (unless (or destructure-p (eq l safecopy) (go LOSE)))
       (setq l safecopy)
       (unless (dolist (key the-keys t)
                 (when (setq m (cdr (memq key l)))
                   (if (memq key m) (return))))
         (go LOSE))
       (if (null l) (go WIN))
       (setq opttail (memq '&optional l))
       (setq m (or (memq '&rest l)
                   (unless destructure-p (memq '&lexpr l))))
       (setq n (if destructure-p (memq '&body l)))
       (if (and m n) (go LOSE) (setq resttail (or m n)))
       (setq keytail (memq '&key l))
       (if (and (setq allowothertail (memq '&allow-other-keys l))
                (not keytail))
         (go LOSE))
       (if (and (eq (car resttail) '&lexpr)
                (or keytail opttail))
         (go lose))
       (setq auxtail (memq '&aux l))
       (loop
         (when (null l) (go WIN))
         (when (or (eq l opttail)
                   (eq l resttail)
                   (eq l keytail)
                   (eq l allowothertail)
                   (eq l auxtail))
           (return))
         (setq sym (pop l))
         (unless (and (req-arg-p sym destructure-p)
                      (or (proclaimed-ignore-p sym)
                          (and destructure-p (null sym))
                          (not (memq sym req)))) ; duplicate required args
           (go LOSE))
         (push sym req))
       (when (eq l opttail)
         (setq l (cdr l))
         (loop
           (when (null l) (go WIN))
           (when (or (eq l resttail)
                     (eq l keytail)
                     (eq l allowothertail)
                     (eq l auxtail))
             (return))
           (unless (opt-arg-p (pop l) destructure-p)
             (go LOSE))))
       (when (eq l resttail)
         (setq l (cdr l))
         (when (or (null l)
                   (eq l opttail)
                   (eq l keytail)
                   (eq l allowothertail)
                   (eq l auxtail))
           (go LOSE))
         (unless (req-arg-p (pop l) destructure-p) (go LOSE)))
       (unless (or (eq l keytail)
                   (eq l auxtail))
         (go LOSE))
       (when (eq l keytail)
         (pop l)
         (loop
           (when (null l) (go WIN))
           (when (or (eq l opttail)
                     (eq l resttail))
             (go LOSE))
           (when (or (eq l auxtail) (setq n (eq l allowothertail)))
             (if n (setq l (cdr l)))
             (return))
           (unless (key-arg-p (pop l) destructure-p) (go LOSE))))
       (when (eq l auxtail)
         (setq l (cdr l))
         (loop
           (when (null l) (go WIN))
           (when (or (eq l opttail)
                     (eq l resttail)
                     (eq l keytail))
             (go LOSE))
           (unless (pair-arg-p (pop l)) (go LOSE))))
       (when l (go LOSE))
     WIN
       (return (values
                t
                (nreverse req)
                (or opttail resttail keytail auxtail)
                (or resttail keytail auxtail)
                (or keytail auxtail)
                auxtail
                safecopy
                whole
                *structured-lambda-list*))
     LOSE
       (return (values nil nil nil nil nil nil nil nil nil nil)))))

(defun normalize-lambda-list (x &optional whole-p env-p)
  (let* ((y x) whole env envtail head)
    (setq
     x
     (loop
       (when (atom y)
         (if (or (null y) (eq x y))  (return x))
         (setq x (copy-list x) y x)
         (return
           (loop
             (when (atom (cdr y))
               (rplacd y (list '&rest (cdr y)))
               (return x))
             (setq y (cdr y)))))
       (setq y (cdr y))))
    (when env-p
      ;; Trapped in a world it never made ...
      (when (setq y (memq '&environment x))
        (setq envtail (cddr y)
              env (cadr y))
        (cond ((eq y x)
               (setq x envtail))
              (t
               (dolist (v x)
                 (if (eq v '&environment)
                   (return)
                   (push v head)))
               (setq x (nconc (nreverse head) envtail) y (car envtail))))))
    (when (and whole-p
               (eq (car x) '&whole)
               (cadr x))
      (setq whole (cadr x) x (cddr x)))
    (values x whole env)))

(defun pair-arg-p
    (thing &optional lambda-list-ok supplied-p-ok keyword-nesting-ok)
  (or (symbol-arg-p thing lambda-list-ok)
      (and (consp thing)
           (or (null (cdr thing))
               (and (consp (cdr thing))
                    (or (null (cddr thing))
                        (and supplied-p-ok
                             (consp (cddr thing))
                             (null (cdddr thing))))))
           (if (not keyword-nesting-ok)
             (req-arg-p (car thing) lambda-list-ok)
             (or (symbol-arg-p (car thing) lambda-list-ok)
                 (and (consp (setq thing (car thing)))
                      (consp (cdr thing))
                      (null (cddr thing))
                      (car thing)
                      (symbolp (car thing))
                      (req-arg-p (cadr thing) lambda-list-ok)))))))

(defun req-arg-p (thing &optional lambda-list-ok)
  (or
   (symbol-arg-p thing lambda-list-ok)
   (lambda-list-arg-p thing lambda-list-ok)))

(defun symbol-arg-p (thing nil-ok)
  (and
   (symbolp thing)
   (or thing nil-ok)
   (not (memq thing lambda-list-keywords))))

(defun opt-arg-p (thing &optional lambda-ok)
  (pair-arg-p thing lambda-ok t nil))

(defun key-arg-p (thing &optional lambda-ok)
  (pair-arg-p thing lambda-ok t t))


(defun memq (item list)
  #-:lispworks
  (do* ((tail list (cdr tail)))
       ((null tail))
    (if (eq item (car tail))
      (return tail)))
  #+:lispworks
  (system:memq item list))


(defvar *nx-proclaimed-ignore* '())

(defun proclaimed-ignore-p (sym)
  (cdr (assq sym *nx-proclaimed-ignore*)))

(defvar *structured-lambda-list* nil)

(defun lambda-list-arg-p (thing lambda-list-ok)
  (and
   lambda-list-ok
   (listp thing)
   (if (verify-lambda-list thing t t)
     (setq *structured-lambda-list* t))))


(defun assq (item list)
  #-:lispworks
  (dolist (pair list)
    (when (and pair (eq (car pair) item)) (return pair)))
  #+:lispworks
  (system:assq item list))


;;;; RECORD-ARGLIST

(defun record-arglist (name args)
  (declare (ignore name args)))

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

;;;; WHITESPACEP

(defun whitespacep (char)
  (member char '(#\Space #\Newline #\Backspace #\Tab
                 #\Linefeed #\Page #\Return #\Rubout)))

;;;; PROCLAIMED-SPECIAL-P

(defun proclaimed-special-p (symbol)
  (declare (ignore symbol))
  t)

;;;; GET-TYPE-PREDICATE

(defun get-type-predicate (type)
  (declare (ignore type))
  t)

;;;; REQUIRE-TYPE

(defun require-type (arg type)
  (if (typep arg type)
    arg
    (error "Argument ~S is not of type ~S." arg type)))

;;;; *LOADING-FILE-SOURCE-FILE*

(defvar *loading-file-source-file* nil)

;;;; NEQ

(defun neq (x y)
  (not (eq x y)))

;;;; *SAVE-SOURCE-LOCATIONS*

(defvar *save-source-locations* nil)

;;;; NFUNCTION

(defmacro nfunction (name lambda-expression)
  `(alexandria:named-lambda ,name ,@(cdr lambda-expression)))

;;;; RECORD-SOURCE-FILE

(defun record-source-file (name def-type &optional source)
  (declare (ignore name def-type source)))

;;;; PREPARE-TO-DESTRUCTURE

(defun prepare-to-destructure (list lambda-list min max)
  (if (if max
        (and (alexandria:proper-list-p list)
             (let* ((len (length list)))
               (<= min len max)))
        (do* ((tail list (cdr tail))
              (n min (1- n)))
             ((zerop n) t)
          (when (atom tail)
            (return))))
    list
    (let* ((reason
             (if max
               (if (not (alexandria:proper-list-p list))
                 "it is not a proper list"
                 (let* ((len (length list)))
                   (if (eql min max)
                     (format nil "it contains ~d elements, and exactly ~d are expected" len min)
                     (if (< len min)
                       (format nil "it contains ~d elements, and at least ~d are expected" len min)
                       (format nil "it contains ~d elements, and at most ~d are expected" len max)))))
               (format nil "it does not contain at least ~d elements" min))))
      (error
       "~s can't be destructured against the lambda list ~s, because ~a."
       list lambda-list reason))))
