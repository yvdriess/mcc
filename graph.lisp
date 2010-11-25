;; (defstruct item-node
;;   consumes
;;   produces)

;; (defstruct tag-node
;;   prescribes
;;   produces)

;; (defstruct graph
;;   nodes
;;   inputs
;;   outputs)

;; (defmethod merge-graph ((graph-produce graph) (graph-consume graph))
;; ;  (intersection (graph-outputs graph-produce)
;; ;		(graph-inputs graph-consume))
  
;;   )

;; (defmethod generate-cnc-header (graph graph)
;;   ;...
;;   )

;(compile-to-graph (parse-sequence '((E 1 2) (M 1 0) (X 2 (s 1)))))


;; (defpackage mc-compiler 
;;   (:use :cl)
;;   (:export compile-to-graph))

;; (in-package :mc-compiler)

;;;; MACRO HELPERS


;;; Allows the definition of symbol lists as constants without the compiler complaining
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(declaim (optimize (speed 0) (debug 3) (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;     ABSTRACT GRAMMAR    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +entanglement-symbol+ 'E)
(defconstant +measurement-symbol+  'M)
(defconstant +correction-X-symbol+ 'X)
(defconstant +correction-Z-symbol+ 'Z)

(defconstant +signal-qubit-symbol+ 'q)
(defconstant +signal-sum-symbol+ '+)

(define-constant +angle-constants+
  `((pi/2  . ,(/ pi 2))
    (pi/4  . ,(/ pi 4))
    (pi/8  . ,(/ pi 8))
    (-pi   . ,(- pi))
    (-pi/2 . ,(- (/ pi 2)))
    (-pi/4 . ,(- (/ pi 4)))
    (-pi/8 . ,(- (/ pi 8)))))

(defun qubit-id-p (exp)
  (typep exp 'unsigned-byte))

(defmacro eval-lisp-angle (angle-expr)
  `(progv
       ',(loop for clause in +angle-constants+
	    collect (car clause))
       ',(loop for clause in +angle-constants+
	    collect (cdr clause))
     (eval ,angle-expr)))
              
;; number | built-in-angle | lisp-form
(defun eval-angle (angle)
  (typecase angle
    (number angle)
    (symbol (let ((angle-constant-entry (assoc angle +angle-constants+)))
	      (if angle-constant-entry
		  (cdr angle-constant-entry)
		  (error "Invalid measurment angle: ~A~%" angle))))
    (cons (eval-angle (eval-lisp-angle angle)))
    (otherwise (error "Measurement operation does not have a valid measurement angle: ~A~%" angle))))

(defstruct ag-signal
  (flag nil :type boolean) ;flag to flip the signal, this is like MC's (+ 1 ...)
  lookup-flags
  qubit-flags)

(defun make-default-ag-signal ()
  (make-ag-signal))

(defstruct ag-operation)

(defstruct (ag-entanglement (:include ag-operation))
  (qubit-1 0 :type unsigned-byte)
  (qubit-2 0 :type unsigned-byte))

(defstruct (ag-measurement (:include ag-operation))
  qubit
  angle
  (s-signal (make-default-ag-signal) :type ag-signal)
  (t-signal (make-default-ag-signal) :type ag-signal))

(defstruct (ag-correction (:include ag-operation))
  qubit
  (s-signal (make-default-ag-signal) :type ag-signal))

(defstruct (ag-X-correction (:include ag-correction)))
(defstruct (ag-Z-correction (:include ag-correction)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;          PARSER         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-sequence (expression-list)
  "Parse a sequence of operations, passed as a list of symbols"
  (loop for exp in expression-list
       collect (parse-operation exp)))

(defun parse-operation (exp)
  (assert exp (exp) "Cannot parse empty expression")
  (let ((operation-name (intern (symbol-name (car exp)))))
    (ecase operation-name
      (#.+entanglement-symbol+ (parse-entanglement exp))
      (#.+measurement-symbol+  (parse-measurement  exp))
      (#.+correction-X-symbol+ (parse-correction exp))
      (#.+correction-Z-symbol+ (parse-correction exp)))))

(defun parse-entanglement (exp)
  (let ((args (cdr exp)))
    (assert (eq (length args) 2) 
	    (exp) "Entanglement operation didn't get 2 arguments")
    (assert (every #'qubit-id-p args) 
	    (exp) "Entanglement operation got non-number argument")
    (let ((qubit-1 (first args))
	  (qubit-2 (second args)))
      (make-ag-entanglement :qubit-1 (max qubit-1 qubit-2) 
			    :qubit-2 (min qubit-1 qubit-2)))))

(defun parse-correction (exp)
  (let ((args (cdr exp)))
    (assert (or (= (length args) 1) 
		(= (length args) 2)) 
	    (args) "Correction operation needs at least a qubit identifier: ~A" exp)
    (let* ((qubit (parse-qubit (first args)))
	   (s-signal (if (= (length args) 2)
			 (parse-signal (second args))
			 (make-default-ag-signal))))
      (ecase (car exp)
	(#.+correction-X-symbol+ (make-ag-X-correction :qubit qubit 
						       :s-signal s-signal))
	(#.+correction-Z-symbol+ (make-ag-Z-correction :qubit qubit 
						       :s-signal s-signal))))))

(defun parse-measurement (exp)
  (let* ((args (cdr exp)))
    (assert (between (length args) 1 4)) ; at least a qubit id, rest is optional/default
    (let ((qubit (parse-measurement-qubit args))
	  (angle (parse-measurement-angle args))
	  (s-signal (parse-measurement-s-signal args))
	  (t-signal (parse-measurement-t-signal args)))
      (make-ag-measurement :qubit qubit :angle angle :s-signal s-signal :t-signal t-signal))))

(defun parse-measurement-qubit (args)    
  (assert (qubit-id-p (first args)) 
	  (args) "Measurement operation did not get a valid qubit identifier")
  (first args))

(defun parse-measurement-angle (args)
  (if (>= (length args) 2)
      (eval-angle (second args))
      0))

(defun parse-measurement-s-signal (args)
  (if (>= (length args) 3)
      (parse-signal (third args))
      (make-default-ag-signal)))

(defun parse-measurement-t-signal (args)
  (if (= (length args) 4)
      (parse-signal (fourth args))
      (make-default-ag-signal)))

(defun ag-signal-flip-flag (ag-signal) 
  (setf (ag-signal-flag ag-signal)
	(not (ag-signal-flag ag-signal)))
  ag-signal)

(defun add-ag-signal-lookup (lookup-name ag-signal)
  (declare (type symbol lookup-name))
  (declare (type ag-signal ag-signal))
  (pushnew lookup-name (ag-signal-lookup-flags ag-signal))
  ag-signal)

(defun add-ag-signal-dependency (qubit-id ag-signal)
  (declare (type ag-signal ag-signal))
  (declare (type unsigned-byte qubit-id))
  (pushnew qubit-id (ag-signal-qubit-flags ag-signal))
  ag-signal)


;; <signal> ::= <identifier> | 0 | 1 | (q <qubit>) | (+ {<signal>}+ )
;; <idenfifier> ::= _symbol_
;; <qubit>  ::= _unsigned-byte_
(defun parse-signal (arg &optional ag-signal)
  "Parses symbol list and returns an ag-signal object,
Syntax:  <identifier> | 0 | 1 | (q <qubit>) | (+ {<signal>}+ )"
  (let ((ag-signal (if ag-signal ag-signal (make-ag-signal))))
    (etypecase arg
      (bit (unless (zerop arg) (ag-signal-flip-flag ag-signal)))
      (symbol (add-ag-signal-lookup arg ag-signal))
      (list (cond ((and (eq (first arg) +signal-qubit-symbol+) ; (q <qubit>)
			(= (length arg) 2))
		   (add-ag-signal-dependency (parse-qubit (second arg))
					     ag-signal))
		  ((and (eq (first arg) +signal-sum-symbol+) ; (+ {<signal>}+)
			(>= (length arg) 2))
		   (loop for sub-expr in (rest arg)
		      do (setf ag-signal (parse-signal sub-expr ag-signal))))
		  (t (error "Cannot parse signal: ~A~%" arg)))))
    ag-signal))

(defun parse-qubit (arg)
  (assert (typep arg 'unsigned-byte) (arg) 
	  "Not a qubit identifier, expected an unsigned-byte and got: ~A~%"
	  arg)
  arg)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   COMPILER INTERFACE   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-to-graph (sequence)
  (loop for operation in sequence
     with program-graph = (empty-program-graph)
     do (setf program-graph (compile-operation operation program-graph))
	 finally (return program-graph)))

(defun compile-operation (operation program-graph)
  (let ((input-nodes (find-input-nodes operation program-graph)))
    (merge-program-graphs program-graph 
			  (make-operation-graph operation input-nodes))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;     GRAPH OPERATIONS    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct node
  content
  upstream-nodes
  downstream-nodes)

(defstruct graph
  nodes
  input-nodes
  output-nodes)

;;; GENERAL GRAPH OPERATIONS

(defun empty-program-graph ()
  (make-graph))

(defun merge-program-graphs (graph-upstream graph-downstream)
  "Merges two _already connected_ graphs, returns a new graph object"
  (let ((connected-nodes (intersection (graph-output-nodes graph-upstream)
				       (graph-input-nodes graph-downstream))))
    (make-graph :nodes (union (graph-nodes graph-upstream)
			      (graph-nodes graph-downstream))
		:input-nodes (union (graph-input-nodes graph-upstream)
				    (set-difference (graph-input-nodes graph-downstream)
						    connected-nodes))
		:output-nodes (union (graph-output-nodes graph-downstream)
				     (set-difference (graph-output-nodes graph-upstream)
						     connected-nodes)))))


(defgeneric connect (upstream downstream))

(defmethod connect ((upstream node) (downstream node))
  (pushnew downstream (node-downstream-nodes upstream))
  (pushnew upstream (node-upstream-nodes downstream)))

(defmethod connect ((upstream graph) (downstream graph))
  (setf (graph-output-nodes upstream)
	(union (graph-output-nodes upstream)
	       (graph-input-nodes downstream)))
  (setf (graph-input-nodes downstream) 
	(union (graph-input-nodes downstream) 
	       (graph-output-nodes upstream))))

(defmethod connect ((upstream node) (downstream graph))
  (setf (node-downstream-nodes upstream) 
	(union (node-downstream-nodes upstream) 
	       (graph-input-nodes downstream)))
  (pushnew upstream (graph-input-nodes downstream)))

(defmethod connect ((upstream graph) (downstream node))
  (pushnew downstream (graph-output-nodes upstream))
  (setf (node-upstream-nodes downstream)
	(union (node-upstream-nodes downstream)
	       (graph-output-nodes upstream))))

;;;
;;; OPERATION-SPECIFIC GRAPH OPERATIONS
;;;

(defgeneric find-input-nodes (operation program-graph))

(defgeneric make-operation-graph (operation input-nodes))

;;
;; entanglement
;;

(defmethod find-input-nodes ((operation ag-entanglement) program-graph)
  
)


(defun pipeline-connect (upstream-stage downstream-pipeline-stages)
  "Connects all nodes from one stage (a list of nodes) with the other. (A B) (C D) becomes A->C A->D B->C B->D
Recursively doing the same with the other stages"
  (declare (type list upstream-stage downstream-pipeline-stages))
  (assert upstream-stage (upstream-stage) "Trying to connect nil into a pipeline, was expecting a list of nodes")
  (when downstream-pipeline-stages
    (let ((downstream-stage (first downstream-pipeline-stages)))
      (loop for node in upstream-stage
	 do (setf (node-downstream-nodes node) downstream-stage))
      (loop for node in downstream-stage
	 do (setf (node-upstream-nodes node) upstream-stage))
      (pipeline-connect downstream-stage 
			(rest downstream-pipeline-stages)))))

(defgeneric make-computation-node (ag-operation))

(defmethod make-computation-node ((operation ag-entanglement))
  (make-node :content operation))

(defun make-kronecker-product-node ()
  (make-node :content (gensym "Kron")))

(defun make-entanglement-node
   (make-node :content (gensym "Entangle")) )

(defmethod make-operation-graph ((operation ag-entanglement) input-nodes)
  (let* ((output-node (make-tangle))
	 (pipeline (case (length input-nodes)
		     (1 (list input-nodes 
			      (list (make-computation-node operation)) 
			      (list output-node)))
		     (2 (list input-nodes 
			      (list (make-kronecker-product-node)) 
			      (list (make-tangle)) 
			      (list (make-computation-node operation))
			      (list output-node)))
		     (otherwise (error "Entanglement operation got more than 2 input nodes, this shouldn't happen.")))))
    (pipeline-connect )
    (make-graph :nodes pipeline
		:input-nodes input-nodes
		:output-nodes (list output-node))))
  

;;;;  UTILITY FUNCTIONS

(defun between (i min max)
  (and (>= i min)
       (<= i max)))

;;;; TESTS

(defvar *test-1-result* 
  (list (make-AG-ENTANGLEMENT :QUBIT-1 2 :QUBIT-2 1)
	(make-AG-MEASUREMENT
	   :QUBIT 2
	   :ANGLE #C(0.0d0 1.2533141373155001d0)
	   :S-SIGNAL (make-AG-SIGNAL :FLAG NIL :LOOKUP-FLAGS NIL :QUBIT-FLAGS '(3))
	   :T-SIGNAL (make-AG-SIGNAL
			:FLAG T
			:LOOKUP-FLAGS '(BC A)
			:QUBIT-FLAGS '(5 3 2)))
	(make-AG-X-CORRECTION
	   :QUBIT 3
	   :S-SIGNAL (make-AG-SIGNAL :FLAG NIL :LOOKUP-FLAGS NIL :QUBIT-FLAGS NIL))
	(make-AG-Z-CORRECTION
	   :QUBIT 1
	   :S-SIGNAL (make-AG-SIGNAL :FLAG NIL :LOOKUP-FLAGS NIL :QUBIT-FLAGS '(2)))))

(defun test-all ()
  (assert (equalp (parse-sequence '((E 1 2) 
				    (M 2 (sqrt -pi/2) (q 3) (+ 1 a 1 bc (+ 1 (q 2)) (q 3) (q 5)))
				    (X 3)
				    (Z 1 (q 2))))
		  *test-1-result*))
  t)

(test-all)