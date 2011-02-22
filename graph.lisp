
;;;; File Organisation:
;;;; 1. Abstract Grammar
;;;; 2. Parser
;;;;  2.1 AG datastructs
;;;;  2.2 parse operations
;;;; 3. Graph Compiler
;;;;  3.1 graph datastructs
;;;;  3.2 compiler datastructs
;;;;  3.3 make-operation-graph
;;;;  3.4 find-input-nodes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload 'alexandria)
  (declaim (optimize (speed 0) (debug 3) (safety 3))))

(defpackage :mc-graph-compiler
  (:nicknames :mcc)
  (:use :cl)
  (:import-from alexandria with-gensyms if-let compose)
  (:export compile-mc ))

(eval-when (:compile-toplevel :load-toplevel)
 (declaim (optimize (speed 0) (debug 3) (safety 3))))

(in-package :mcc)

(defpackage :parsed-symbols
  (:nicknames :parsed))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; these get used at macro-expand time
  (defun symbol-to-string (sym)
    (string-downcase (symbol-name sym)))
  (defun symbol-list-to-string (symlist)
    (mapcar #'symbol-to-string symlist))
  (defvar *debug* T))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 1.  ABSTRACT GRAMMAR    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Read-time constants

(defmacro define-constant (name value &optional doc)
  "Allows the definition of symbol lists as constants without the compiler complaining"
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defconstant +parse-package+ #.(find-package :mcc))
(defconstant +entanglement-symbol+ 'E)
(defconstant +measurement-symbol+  'M)
(defconstant +correction-X-symbol+ 'X)
(defconstant +correction-Z-symbol+ 'Z)

(defconstant +signal-qubit-symbol+ 'q)
(defconstant +signal-sum-symbol+ '+)

(defconstant +qubit-id-type+ 'unsigned-byte)

(define-constant +angle-constants+
  `((pi/2  . ,(/ pi 2))
    (pi/4  . ,(/ pi 4))
    (pi/8  . ,(/ pi 8))
    (-pi   . ,(- pi))
    (-pi/2 . ,(- (/ pi 2)))
    (-pi/4 . ,(- (/ pi 4)))
    (-pi/8 . ,(- (/ pi 8)))))

(defun intern-exp (exp)
  (typecase exp
    (cons (cons (intern-exp (car exp))
		(intern-exp (cdr exp))))
    (symbol (intern (symbol-name exp) #.+parse-package+))
    (t exp)))

(defun qubit-id-p (exp)
  (typep exp +qubit-id-type+))

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
		  (eval-lisp-angle angle)
		  ;(error "Invalid measurment angle: ~A~%" angle)
		  )))
    (cons (eval-angle (eval-lisp-angle angle)))
    (otherwise (error "Measurement operation does not have a valid measurement angle: ~A~%" angle))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 2.       PARSER         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 2.1 Abstract Grammar Datastructures

(defstruct ag-signal
  (flag nil :type boolean) ;flag to flip the signal, this is like MC's (+ 1 ...)
  lookup-flags
  qubit-flags)

(defstruct ag-operation)

(defstruct (ag-entanglement (:include ag-operation))
  (qubit-1 0 :type #.+qubit-id-type+)
  (qubit-2 0 :type #.+qubit-id-type+))

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

(defun make-default-ag-signal ()
  (make-ag-signal))

;;; datastructure manipulations

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
  (declare (type #.+qubit-id-type+ qubit-id))
  (pushnew qubit-id (ag-signal-qubit-flags ag-signal))
  ag-signal)


;;; 2.2 Parse operations

;; This is the parser's entry point
(defun parse-sequence (expression-list)
  "Parse a sequence of operations, passed as a list of symbols"
  (loop for exp in expression-list
       collect (parse-operation exp)))

(defun parse-operation (exp)
  (assert exp (exp) "Cannot parse empty expression")
  (let ((operation-name (intern (symbol-name (car exp)) +parse-package+)))
    (ecase operation-name
      (#.+entanglement-symbol+ (parse-entanglement exp))
      (#.+measurement-symbol+  (parse-measurement  exp))
      (#.+correction-X-symbol+ (parse-correction   exp))
      (#.+correction-Z-symbol+ (parse-correction   exp)))))

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
      (ecase (intern (symbol-name (car exp)) #.+parse-package+)
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
      (eval-angle (intern-exp (second args)))
      0))

(defun parse-measurement-s-signal (args)
  (if (>= (length args) 3)
      (parse-signal (third args))
      (make-default-ag-signal)))

(defun parse-measurement-t-signal (args)
  (if (= (length args) 4)
      (parse-signal (fourth args))
      (make-default-ag-signal)))


;; <signal> ::= <identifier> | 0 | 1 | (q <qubit>) | (+ {<signal>}+ )
;; <idenfifier> ::= _symbol_
;; <qubit>  ::= _unsigned-byte_
(defun parse-signal (arg &optional ag-signal)
  "Parses symbol list and returns an ag-signal object,
Syntax:  <identifier> | 0 | 1 | (q <qubit>) | (+ {<signal>}+ )"
  (let ((ag-signal (if ag-signal ag-signal (make-ag-signal))))
    (etypecase arg
      (bit (unless (zerop arg) (ag-signal-flip-flag ag-signal)))
      (symbol (add-ag-signal-lookup (intern (symbol-name arg) +parse-package+)
				    ag-signal))
      (list (assert (symbolp (first arg)))
	    (let ((operand (intern (symbol-name (first arg)) +parse-package+)))
	      (cond ((and (eq operand +signal-qubit-symbol+) ; (q <qubit>)
			  (= (length arg) 2))
		     (add-ag-signal-dependency (parse-qubit (second arg))
					       ag-signal))
		    ((and (eq operand +signal-sum-symbol+) ; (+ {<signal>}+)
			  (>= (length arg) 2))
		     (loop for sub-expr in (rest arg)
			do (setf ag-signal (parse-signal sub-expr ag-signal))))
		    (t (error "Cannot parse signal: ~A~%" arg))))))
    ag-signal))

(defun parse-qubit (arg)
  (assert (typep arg +qubit-id-type+) (arg) 
	  "Not a qubit identifier, expected an unsigned-byte and got: ~A~%"
	  arg)
  arg)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 3.  Graph Compiler    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 3.1 Graph Datastructures & Operations

(defstruct node
  (label (gensym))
  content
  upstream-nodes
  downstream-nodes)

(defstruct graph
  nodes
  input-nodes
  output-nodes)

;; graph operations

(defun make-empty-graph ()
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

(defun input-node-p (node graph)
  (member node (graph-input-nodes graph) :test #'equal))
(defun output-node-p (node graph)
  (member node (graph-output-nodes graph) :test #'equal))

(defun add-input-node (node graph)
  (pushnew node (graph-input-nodes graph)))
(defun add-output-node (node graph)
  (pushnew node (graph-output-nodes graph)))
(defun add-node (node graph)
  (pushnew node (graph-nodes graph)))

;; (defgeneric connect (upstream downstream))

;; (defmethod connect ((upstream node) (downstream node))
;;   (pushnew downstream (node-downstream-nodes upstream))
;;   (pushnew upstream (node-upstream-nodes downstream)))

;; (defmethod connect ((upstream graph) (downstream graph))
;;   (setf (graph-output-nodes upstream)
;; 	(union (graph-output-nodes upstream)
;; 	       (graph-input-nodes downstream)))
;;   (setf (graph-input-nodes downstream) 
;; 	(union (graph-input-nodes downstream) 
;; 	       (graph-output-nodes upstream))))

;; (defmethod connect ((upstream node) (downstream graph))
;;   (setf (node-downstream-nodes upstream) 
;; 	(union (node-downstream-nodes upstream) 
;; 	       (graph-input-nodes downstream)))
;;   (pushnew upstream (graph-input-nodes downstream)))

;; (defmethod connect ((upstream graph) (downstream node))
;;   (pushnew downstream (graph-output-nodes upstream))
;;   (setf (node-upstream-nodes downstream)
;; 	(union (node-upstream-nodes downstream)
;; 	       (graph-output-nodes upstream))))


(defun stage (obj)
  "Returns a simple list of nodes, when a single node is passed a list is wrapped around it."
  (typecase obj
    (node (list obj))
    (list (assert (every #'node-p obj))
	  obj)
    (t (error "A pipeline stage expects either a node or a list of nodes"))))

(defun pipeline (&rest stages)
  "A pipeline is a simple list of stages (a list of nodes)."
  stages)

(defun pipeline-connect (pipeline-stages)
  "Destructively connects all nodes from one stage (a list of nodes)
with the other. (A B) (C D) becomes A->C A->D B->C B->D Recursively
doing the same with the other stages."
  (when pipeline-stages
    (let ((upstream-stage (first pipeline-stages)) 
	  (downstream-pipeline-stages (rest pipeline-stages)))
      (assert upstream-stage (upstream-stage) "Trying to connect nil into a pipeline, was expecting a list of nodes")
      (when downstream-pipeline-stages
	(let ((downstream-stage (first downstream-pipeline-stages)))
	  (loop for node in upstream-stage
	     do (setf (node-downstream-nodes node)
		      (union (node-downstream-nodes node)
			     downstream-stage)))
	  (loop for node in downstream-stage
	     do (setf (node-upstream-nodes node) 
		      (union (node-upstream-nodes node) 
			     upstream-stage)))
	  (pipeline-connect downstream-pipeline-stages))))))

(defun pipeline-to-graph (pipeline-stages)
  (pipeline-connect pipeline-stages)
  (make-graph :nodes (loop for nodes in pipeline-stages
			  appending nodes)
	      :input-nodes (first pipeline-stages)
	      :output-nodes (first (last pipeline-stages))))

(defun find-output-node (graph pred-func)
  "Returns the first output node that returns true when applying the
predicate to its _content_."
  (declare (type graph graph))
  (declare (type function pred-func))
  (some #'(lambda (node)
	    (let ((content (node-content node)))
	      (when (funcall pred-func content)
		node)))
	(graph-output-nodes graph)))


;;; 3.2 Compiler Datastructures

;;; 3.2.1 Tangle

(defstruct tangle
  "Simple list of qubit identifiers"
  qubits)

(defun merge-tangles (tangle-1 tangle-2)
  "Returns a new tangle containing the qubits of both tangles, in order."
  #+*debug*(assert (null (intersection (tangle-qubits tangle-1)
			      (tangle-qubits tangle-2))))
  (make-tangle :qubits (append (tangle-qubits tangle-1)
			       (tangle-qubits tangle-2))))

(defun find-qubit (tangle qubit)
  "Returns nil if the qubit could not be found in the tangle, non-nil otherwise."
  (declare (type tangle tangle))
  (declare (type #.+qubit-id-type+ qubit))
  (find qubit (tangle-qubits tangle)))

(defun qubit-tensor-index (qubit tangle)
  "Returns the tensor-index (power of two) of the given qubit in a tangle."
  (let ((tangle-position (position qubit (tangle-qubits tangle))))
    (if tangle-position
	(expt 2 tangle-position)
	(error "Could not find Qubit ~A in Tangle ~A~%" qubit tangle))))


;;; 3.2.2 Signal Map

(defstruct signal-map
  input-signals
  qubit-signals)

(defun make-empty-signal-map () 
  (make-signal-map))

;;; 3.2.3 Operation-specific manipulators

;; Datastructures (see also ag-operations)

(defstruct operation)

(defstruct (kronecker-operation (:include operation))
  qubit-1
  qubit-2)

(defgeneric qubit-dependencies (operation))
(defmethod qubit-dependencies ((operation ag-measurement))
  (list (ag-measurement-qubit operation)))
(defmethod qubit-dependencies ((operation ag-entanglement))
  (list (ag-entanglement-qubit-1 operation)
	(ag-entanglement-qubit-2 operation)))

(defgeneric qubit-signal-dependencies (operation))
(defmethod qubit-signal-dependencies ((operation ag-measurement))
    (append (ag-signal-qubit-flags (ag-measurement-s-signal operation))
	    (ag-signal-qubit-flags (ag-measurement-t-signal operation))))
(defmethod qubit-signal-dependencies ((operation ag-correction))
  (ag-signal-qubit-flags (ag-correction-s-signal operation)))

(defgeneric input-signal-dependencies (operation))
(defmethod input-signal-dependencies ((operation ag-measurement))
  (append (ag-signal-lookup-flags (ag-measurement-s-signal operation))
	  (ag-signal-lookup-flags (ag-measurement-t-signal operation))))
(defmethod input-signal-dependencies ((operation ag-correction))
  (ag-signal-lookup-flags (ag-correction-s-signal operation)))

(defun ensure-tangle-node (node operation)
  (if node
      node
      (make-data-node (make-tangle :qubits (qubit-dependencies operation)))))

(defun ensure-signal-map-node (node operation)
  (if node
      node
      (progn (when (qubit-signal-dependencies operation)
	       (error "Measurement has dangling qubit dependencies, it
depends on qubit signals that weren't added to the signal map. Did it
get added in the wrong order?"))
	     (make-data-node (make-signal-map :input-signals (input-signal-dependencies operation)
					      :qubit-signals (qubit-signal-dependencies operation))))))

;;; 3.3 MAKE-OPERATION-GRAPH

(defgeneric make-operation-graph (operation input-nodes)
  (:documentation "Returns a program graph with operation and data
nodes representing the given (ag-)operation."))
(defmethod make-operation-graph (operation input-nodes)
  (declare (type (or operation ag-operation operation)))
  (pipeline-to-graph (pipeline (stage input-nodes)
			       (stage (make-computation-node operation)) 
			       (stage (make-output-nodes operation input-nodes)))))

(defmethod make-operation-graph ((operation ag-entanglement) input-nodes)
  (case (length input-nodes)
    (1 (pipeline-to-graph (pipeline (stage input-nodes)
				    (stage (make-computation-node operation))
				    (stage (make-output-nodes operation input-nodes)))))
    (2 (let ((kron-operation-graph (make-operation-graph (make-kronecker-operation :qubit-1 (ag-entanglement-qubit-1 operation)
										   :qubit-2 (ag-entanglement-qubit-2 operation)) 
							 input-nodes)))
	 (merge-program-graphs kron-operation-graph
			       (make-operation-graph operation
						     (graph-output-nodes kron-operation-graph)))))
    (otherwise (error "Entanglement operation got either zero or more
than 2 input nodes, this shouldn't happen."))))

(defmethod make-operation-graph ((operation ag-correction) input-nodes)
  "If a signal-map-node was passed as input (second node) then it is
added to the list of output nodes on the graph, because corrections do not modify signal maps."
  (let ((operation-graph (call-next-method)))
    (when (>= (length input-nodes) 2)      
      (pushnew (second input-nodes) (graph-output-nodes operation-graph)))
    operation-graph))


(defgeneric make-output-nodes (operation input-nodes))
(defmethod make-output-nodes ((operation ag-entanglement) input-nodes)
  (let ((input-tangles (mapcar #'node-content input-nodes)))
    (case (length input-nodes)
      (1 (list (make-data-node (first input-tangles))))
      (2 (list (make-data-node (merge-tangles (first input-tangles)
					      (second input-tangles))))))))

(defmethod make-output-nodes ((operation kronecker-operation) input-nodes)
  (assert (= (length input-nodes) 2))
  (list (make-data-node (merge-tangles (node-content (first input-nodes))
				       (node-content (second input-nodes))))))

(defmethod make-output-nodes ((operation ag-measurement) input-nodes)
  ;We 'know' that find-input-nodes has put a node with a tangle first
  ;and one with a signal-map second
  (let ((input-tangle (node-content (first input-nodes)))
	(input-signal-map (node-content (second input-nodes)))
	(measured-qubit (ag-measurement-qubit operation)))
    ;We return the new nodes in the same order
    (list (make-data-node (make-tangle :qubits (remove measured-qubit
						       (tangle-qubits input-tangle))))
	  (make-data-node (make-signal-map :input-signals (signal-map-input-signals input-signal-map)
					   :qubit-signals (remove measured-qubit
								  (signal-map-qubit-signals input-signal-map)))))))

(defmethod make-output-nodes ((operation ag-correction) input-nodes)
    ;We 'know' that input-nodes has a tangle in the first position
  (let ((input-tangle (node-content (first input-nodes))))
    (list (make-data-node (make-tangle :qubits (tangle-qubits input-tangle))))))


(defgeneric make-computation-node (operation))
(defmethod make-computation-node ((operation ag-entanglement))
  (make-node :label (gensym "OP-Entangle-")
	     :content operation))

(defmethod make-computation-node ((operation kronecker-operation))
  (make-node :label (gensym "OP-Kron-")
	     :content operation))

(defmethod make-computation-node ((operation ag-measurement))
  (make-node :label (gensym "OP-Measure-")
	     :content operation))

(defmethod make-computation-node ((operation ag-X-correction))
  (make-node :label (gensym "OP-X-Correction-")
	     :content operation))

(defmethod make-computation-node ((operation ag-Z-correction))
  (make-node :label (gensym "OP-Z-Correction-")
	     :content operation))


(defgeneric make-data-node (obj))
(defmethod make-data-node ((tangle tangle))
  (make-node :label (gensym "DATA-Tangle-")
	     :content tangle))

(defmethod make-data-node ((signals signal-map))
  (make-node :label (gensym "Signals-")
	     :content signals))

;;; 3.4 FIND-INPUT-NODES

(defgeneric find-input-nodes (operation program-graph)
  (:documentation "Returns the relevant input-nodes for a certain
  operation (in abstract grammar form). Creates new nodes when
  necessary"))
(defmethod find-input-nodes ((operation ag-measurement) program-graph)
  "Returns a list with a tangle node and a signal node (in that
order).  A new tangle or signal node is created when it cannot find
either in the program graph."
					; todo: emulate ag-correction's find-input-nodes signal-map-node handling
  (let ((tangle-node (find-tangle-node program-graph (ag-measurement-qubit operation)))
	(signal-map-node (find-signal-map-node program-graph
					       (qubit-signal-dependencies operation)
					       (input-signal-dependencies operation))))
    (list (ensure-tangle-node tangle-node operation)
	  (ensure-signal-map-node signal-map-node operation))))

(defmethod find-input-nodes ((operation ag-correction) program-graph)
  "Returns a list with a tangle node and (optionally) a signal node (in that
order). A new tangle node is created when it cannot find it in the program graph."
  (let ((tangle-node (find-tangle-node program-graph (ag-correction-qubit operation)))
	(qubit-dep (qubit-signal-dependencies operation))
	(input-dep (input-signal-dependencies operation)))
    (cons (ensure-tangle-node tangle-node operation)
	  (if (or qubit-dep input-dep)
	      (let ((signal-map-node (find-signal-map-node program-graph 
							   qubit-dep
							   input-dep)))
		(if signal-map-node
		    (list signal-map-node)
		    (error "There are qubit dependencies, but cannot find the required signal-map.")))))))

(defmethod find-input-nodes ((operation ag-entanglement) program-graph)
  "Returns one or two tangles. Either it finds no relevant tangles in the program-graph and creates two new tangles,
returns an existing tangle and a created one, returns two existing
tangles or returns one existing tangle."
  (let ((tangle-node-1 (find-tangle-node program-graph
					 (ag-entanglement-qubit-1 operation)))
	(tangle-node-2 (find-tangle-node program-graph
					 (ag-entanglement-qubit-2 operation))))
					;    (break "args: ~A ~A" tangle-node-1 tangle-node-2)
    (declare (type (or node null) tangle-node-1 tangle-node-2))
    (cond ((and tangle-node-1 tangle-node-2) ; both non-nil
	   (if (equal tangle-node-1 tangle-node-2)
	       (list tangle-node-1)		    ; both equal
	       (list tangle-node-1 tangle-node-2))) ; both different
	  ((equal tangle-node-1 tangle-node-2)	    ; both nil
	   (list (make-data-node (make-tangle :qubits (qubit-dependencies operation)))))
	  (tangle-node-1		; only tangle-node-2 is nil
	   (list tangle-node-1
		 (make-data-node (make-tangle :qubits (list (ag-entanglement-qubit-2 operation))))))
	  (tangle-node-2		; only tangle-node-1 is nil
	   (list (make-data-node (make-tangle :qubits (list (ag-entanglement-qubit-1 operation))))
		 tangle-node-2))
	  ;; ((or tangle-node-1 tangle-node-2)  ; one is nil
	  ;;  (list (ensure-tangle-node tangle-node-1 operation)
	  ;; 	 (ensure-tangle-node tangle-node-2 operation)))
	  (t (error "find-input-nodes failed to find or create the correct tangles")))))

;;; 3.4 Searching in Graph and Node contents

(defun find-tangle-node (graph qubit)
  (find-output-node graph 
		    #'(lambda (content)
			(and (tangle-p content)		   
			     (find-qubit content qubit)))))

(defun find-signal-map-node (graph qubit-signal-dependencies input-signal-dependencies)
  "Returns a signal-map object that contains all the signal dependencies, nil otherwise."
  (declare (ignore qubit-signal-dependencies input-signal-dependencies))
  (find-output-node graph
		    #'(lambda (content)
			(and (signal-map-p content)
			     ;; TODO for now we are assuming all dependencies are met
			     ;; (subsetp qubit-signal-dependencies
			     ;; 	      (signal-map-qubit-signals content))
			     ;; (subsetp input-signal-dependencies
			     ;; 	      (signal-map-input-signals content))
			     ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   COMPILER INTERFACE   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun compile-mc (mc-sequence)
  "Takes valid a QVM program (a MC operation sequence) and compiles it
to a program graph"
  (compile-to-graph (parse-sequence mc-sequence)))

(defun compile-to-graph (sequence)
  "Applies COMPILE-OPERATION to each operation in the sequence,
growing the program-graph."
  (loop for operation in sequence
     with program-graph = (make-empty-graph)
     do (setf program-graph (compile-operation operation program-graph))
     finally (return program-graph)))

(defun compile-operation (operation program-graph)
  "Applies MAKE-OPERATION-GRAPH to each operation in the sequence,
merging the resulting graph with the existing
program-graph. FIND-INPUT-NODES collects the data nodes relevant to
the operation at hand and passes them to MAKE-OPERATION-GRAPH, this is
to ensure the new operation graph has already made the relevant
connections to the program graph."
  (let ((input-nodes (find-input-nodes operation program-graph)))
    (merge-program-graphs program-graph 
			  (make-operation-graph operation input-nodes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Misc. Utility Functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun between (i min max)
  (and (>= i min)
       (<= i max)))

(defun dot-node-label (node graph)
  (declare (ignore graph))
  (let ((content (node-content node)))
    (typecase content
      (tangle (format nil "Tangle { ~{~S ~}}" (tangle-qubits content)))
      (ag-entanglement (format nil "Entangle (~A, ~A)"
			       (ag-entanglement-qubit-1 content)
			       (ag-entanglement-qubit-2 content)))
      (ag-measurement (format nil "Measure ~A" 
			      (ag-measurement-qubit content)))
      (ag-X-correction (format nil "X ~A" (ag-correction-qubit content)))
      (ag-Z-correction (format nil "Z ~A" (ag-correction-qubit content)))
      (kronecker-operation (format nil "Kronecker Product"))
      (otherwise (node-label node)))))

(defun dot-node-shape (node graph)
  (typecase (node-content node)
    (operation "ellipse")
    (ag-operation "ellipse")
    (tangle "rectangle")
    (otherwise (cond ((member node (graph-input-nodes graph))
		      "triangle")
		     ((member node (graph-output-nodes graph))
		      "invtriangle")
		     (t "diamond")))))

(defun show-dot (graph)
  (with-open-file (dotfile
		   #P"/tmp/graph.dot" 
		   :direction :output 
		   :if-does-not-exist :create 
		   :if-exists :supersede)
    (format dotfile "digraph {~%" )
    (loop for node in (graph-nodes graph)
       do (format dotfile "\"~A\" [label=\"~A\",shape=~A];~%" 
		  (node-label node) 
		  (dot-node-label node graph)
		  (dot-node-shape node graph)))
    (loop for node in (graph-nodes graph)
       do (loop for child-node in (node-downstream-nodes node)
	       do (format dotfile "\"~A\" -> \"~A\" [];~%" (node-label node) (node-label child-node))))
    (format dotfile "}~%" )
    )
  ; (sb-ext:run-program "/Applications/Graphviz.app/Contents/MacOS/Graphviz" (list "/tmp/graph.dot"))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        TESTS           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *test-1-result* 
  (list (make-AG-ENTANGLEMENT :QUBIT-1 2 :QUBIT-2 1)
	(make-AG-MEASUREMENT
	   :QUBIT 2
	   :ANGLE 1.2533141373155001d0
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

(defun test-op-graph ()
  (make-operation-graph (make-ag-entanglement :qubit-1 2 :qubit-2 1) (list (make-data-node (make-tangle :qubits (list 2))) 
									   (make-data-node (make-tangle :qubits (list 1 3))))))


(defun test-all ()
  (assert (equalp (parse-sequence '((E 1 2) 
				    (M 2 (sqrt pi/2) (q 3) (+ 1 a 1 bc (+ 1 (q 2)) (q 3) (q 5)))
				    (X 3)
				    (Z 1 (q 2))))
		  *test-1-result*))
  (assert (equalp (find-input-nodes (make-ag-entanglement :qubit-1 1 :qubit-2 3) (test-op-graph))
		  (list (make-tangle :qubits '(3 1 2)))))
  
  t)

;; (find-input-nodes (make-ag-entanglement :qubit-1 1 :qubit-2 3) (make-empty-graph))
;; (find-input-nodes (make-ag-entanglement :qubit-1 5 :qubit-2 2) (test-op-graph))

;(compile-operation (make-ag-entanglement :qubit-1 2 :qubit-2 1) (make-empty-graph))
;(show-dot (compile-operation (make-ag-entanglement :qubit-1 1 :qubit-2 6) (compile-operation (make-ag-entanglement :qubit-1 5 :qubit-2 6) (test-op-graph))))
;(show-dot (compile-to-graph (parse-sequence '((E 1 2) (M 1 0) (X 2 (q 1))))))
;(show-dot (compile-to-graph (parse-sequence '((E 1 2) (E 5 6) (E 3 4) (E 1 5) (E 6 3)))))
;(show-dot (compile-to-graph (parse-sequence '((E 1 2) (E 5 6) (E 3 4) (E 1 5) (E 6 3) (M 1 0) (M 5 pi) (M 3 (sqrt 2) (+ 1 a 1 bc (+ 1 (q 2) (q 5))))))))
;(show-dot (compile-to-graph (parse-sequence '((E 1 2) (E 5 6) (E 3 4) (E 1 5) (E 6 3) (M 1 0) (M 5 pi) (M 3 (sqrt 2) (+ 1 a 1 bc (+ 1 (q 2) (q 5)))) (X 4 )))))
;(show-dot (compile-to-graph (parse-sequence '((E 1 2) (E 5 6) (E 3 4) (E 1 5) (E 6 3) (M 1 0) (M 5 pi) (M 3 (sqrt 2) (+ 1 a 1 bc (+ 1 (q 2) (q 5)))) (Z 6 (q 3)) (X 4) ))))
;(show-dot (compile-to-graph (parse-sequence '((E 1 2) (E 5 6) (E 3 4) (E 1 5) (E 6 3) (M 1 0) (M 5 pi) (M 3 (sqrt 2) (+ 1 a 1 bc (+ 1 (q 2) (q 5)))) (Z 6 (q 3)) (X 4) (X 2 (q 1))))))
;(test-all)

;; Real MC program, produces 3 W-entangled qubits
(defvar *w3-program*
  (reverse 
   `((X 18 (+ 1 (q 1) (q 3) (q 5) (q 7) (q 11) (q 13) (q 15) (q 17)))
     (Z 18 (+ 1 (q 8) (q 14) (q 15) (q 16)))
     (X 10 (+ (q 9) (q 11) (q 13) (q 15) (q 17)))
     (Z 10 (+ 1 (q 8) (q 11) (q 12)))
     (X 6  (+ (q 1) (q 3) (q 5)))
     (Z 6  (+ 1 (q 2) (q 3) (q 4)))
     (M 17 0) (M 16 pi/2) (M 15 pi/4 (q 14)) (M 14 pi/2) (M 13 0)
     (M 12 pi/2) (M 11 -pi/4)
     (M 9 0) (M 8 0) (M 7 0) (M 5 0)
     (M 4 pi/2) (M 3 (acos (sqrt (/ 2 3))) (+ (q 1) (q 2))) (M 2 pi/2) (M 1 0)
     ,@(loop for i from 10 above 1 collect `(E ,(1- i) ,i))
     ,@(loop for i from 18 above 11 collect `(E ,(1- i) ,i))        
     (E 8 13) (E 9 18))))

(defvar *short-w3-program*
  (reverse 
   `(     (M 17 0) (M 16 pi/2) (M 15 pi/4 (q 14)) (M 14 pi/2) (M 13 0)
     (M 12 pi/2) (M 11 -pi/4)
     (M 9 0) (M 8 0) (M 7 0) (M 5 0)
     (M 4 pi/2) (M 3 (acos (sqrt (/ 2 3))) (+ (q 1) (q 2))) (M 2 pi/2) (M 1 0)
     ,@(loop for i from 10 above 1 collect `(E ,(1- i) ,i))
     ,@(loop for i from 18 above 11 collect `(E ,(1- i) ,i))        
     (E 8 13) (E 9 18))))

;(show-dot (compile-mc *w3-program*))

;(show-dot (compile-mc '((E 1 2) (M 1 0) (X 2 (q 1)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   MC GRAPH TO CNC GRAPH   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun mc-graph-to-cnc-graph (mc-graph)
  (let ((cnc-graph (make-graph))
	(swap-table (make-hash-table)))
    ;;  first pass: copy each node, ignore edges
    (loop
       for node in (graph-nodes mc-graph)
       for new-node = (expand-to-cnc-node node)
       do (progn 
	    (setf (gethash node swap-table) new-node)
	    (add-node new-node cnc-graph))
       when (input-node-p node mc-graph)
        do (add-input-node new-node cnc-graph)
       when (output-node-p node mc-graph)
        do (add-output-node new-node cnc-graph))
    ;;  second pass: swap the edges
    (loop for node in (graph-nodes cnc-graph)
       do (setf (node-upstream-nodes node) 
		(swap-nodes (node-upstream-nodes node) swap-table))
       do (setf (node-downstream-nodes node) 
		(swap-nodes (node-downstream-nodes node) swap-table)))
    ;;  third pass: instantiate the content (uses info from edges)
    (loop
       for node in (graph-nodes cnc-graph)
	 do (instantiate-node-content (node-content node) node cnc-graph))
    cnc-graph
    ))

(defun swap-nodes (node-list swap-table)
  (loop for node in node-list
       collecting (if-let ((swapped (gethash node swap-table)))
		    swapped
		    node)))

(defgeneric expand-to-cnc-node (node))

(defmethod expand-to-cnc-node ((node node))
  (let ((new-node (make-node :content (change-node-content (node-content node))
			     :upstream-nodes (node-upstream-nodes node)
			     :downstream-nodes (node-downstream-nodes node))))
    new-node))

;(pprint (mc-graph-to-cnc-graph (compile-mc '((E 1 2) (M 1 0) (X 2 (q 1))))))


(defstruct cnc-tangle
  tangle
  size
  item-collection
  tag-collection)

(defstruct (cnc-input-tangle (:include cnc-tangle))
  generator-tag-collection
  step-kernel)

(defstruct (cnc-output-tangle (:include cnc-tangle))
  step-kernel)

(defstruct cnc-signal
  item-collection)

(defstruct cnc-operation
  input-item-collection
  output-item-collection
  input-tag-collection
  output-tag-collection
  signal-item-collection
  step-kernel
  qubit
  qubit-index
  size
  tuner-name
  tuner-args)

(defstruct (cnc-entanglement-operation (:include cnc-operation))
  qubit-2
  qubit-2-index
  )

(defstruct (cnc-kronecker-operation (:include cnc-operation))
  size-2
  input2-item-collection
  input2-tag-collection)

(defstruct (cnc-measurement-operation (:include cnc-operation))
  angle
  s-signal
  t-signal
  check-step-kernel
  do-m-tag-collection
  )

(defstruct (cnc-correction-operation (:include cnc-operation))
  signal
)

(defstruct item-collection
  name
  (type "amplitude"
	:type string))

(defstruct tag-collection
  (name (symbol-to-string (gensym "tag_")) :type string)
  (type "unsigned int" 
	:type string))

;; ;;;; CNC-OPERATION-STEPS

;; (defgeneric cnc-operation-steps (obj))
;; (defmethod cnc-operation-steps ((obj cnc-operation))
;;   (list (cnc-operation-step-kernel obj)))
;; (defmethod cnc-operation-steps ((obj cnc-measurement-operation))
;;   (list (cnc-operation-step-kernel obj)
;; 	(cnc-measurement-operation-check-step-kernel obj)))



;; ;;;; CNC-OPERATION-TAGS
;; (defgeneric cnc-operation-tags (obj))
;; (defmethod cnc-operation-tags ((obj cnc-operation)))
;; (defmethod cnc-operation-tags ((obj cnc-measurement-operation))
;;   (list (cnc-measurement-operation-do-m-tag-collection obj)))

;;;; CNC-NODE-PRESCRIPTIONS
(defgeneric cnc-node-prescriptions (content node))
(defmethod cnc-node-prescriptions (obj node))
(defmethod cnc-node-prescriptions ((obj cnc-input-tangle) node)
  (list (cons (cnc-input-tangle-step-kernel obj)
	      (cnc-input-tangle-generator-tag-collection obj))))
(defmethod cnc-node-prescriptions ((obj cnc-output-tangle) node)
  (list (cons (cnc-output-tangle-step-kernel obj)
	      (cnc-output-tangle-tag-collection obj))))
(defmethod cnc-node-prescriptions ((obj cnc-operation) node)
  (list (cons (cnc-operation-step-kernel obj)
	      (cnc-operation-input-tag-collection obj))))
(defmethod cnc-node-prescriptions ((obj cnc-measurement-operation) node)
  (list (cons (cnc-measurement-operation-check-step-kernel obj)
	      (cnc-measurement-operation-input-tag-collection obj))
	(cons (cnc-measurement-operation-step-kernel obj)
	      (cnc-measurement-operation-do-m-tag-collection obj))))

(defun make-tangle-name-pair () 
  ;; I want both names to use the same number
  (prog1 (cons (concatenate 'string "tangle_" (number-to-string *gensym-counter*))
	       (concatenate 'string "do_on_tangle_" (number-to-string *gensym-counter*)))
    (incf *gensym-counter*)))

(defun get-cnc-tangle (nodes)
  (if-let ((node (find-if #'cnc-tangle-p nodes
			  :key #'node-content)))
    (node-content node)))

(defun get-cnc-tangles (nodes)
  (if-let ((node-list (remove-if-not #'cnc-tangle-p (mapcar #'node-content nodes))))
    node-list))

(defun get-cnc-signals (nodes)
  (if-let ((node (find-if #'cnc-signal-p nodes
			  :key #'node-content)))
    (node-content node)))

;;;; CHANGE-NODE-CONTENT

(defgeneric change-node-content (obj))

(defmethod change-node-content ((tangle tangle))
  (let ((tangle-name-pair (make-tangle-name-pair)))
    (make-cnc-tangle :tangle tangle
		     :size (expt 2 (length (tangle-qubits tangle)))
		     :item-collection (make-item-collection :name (car tangle-name-pair))
		     :tag-collection (make-tag-collection :name (cdr tangle-name-pair)))))

(let ((the-cnc-signal (make-cnc-signal :item-collection (make-item-collection :name "signals"
									      :type "bool"))))
  (defmethod change-node-content ((signals signal-map))
    the-cnc-signal))

(defmethod change-node-content ((operation ag-entanglement))
  (make-cnc-entanglement-operation :qubit (ag-entanglement-qubit-1 operation)
				   :step-kernel  (get 'E 'kernel)
				   :qubit-2 (ag-entanglement-qubit-2 operation)))

(defmethod change-node-content ((operation kronecker-operation))
  (make-cnc-kronecker-operation :step-kernel (get 'kron 'kernel)))

(defmethod change-node-content ((operation ag-measurement))
  (make-cnc-measurement-operation :qubit (ag-measurement-qubit operation)
				  :angle (ag-measurement-angle operation)
				  :s-signal (ag-measurement-s-signal operation)
				  :t-signal (ag-measurement-t-signal operation)
				  :check-step-kernel (get 'check_M 'kernel)
				  :step-kernel (get 'M 'kernel)
				  :do-m-tag-collection (make-tag-collection :name (symbol-to-string (gensym "do_m_")))))

(defmethod change-node-content ((operation ag-X-correction))
  (make-cnc-correction-operation :qubit (ag-correction-qubit operation)
				 :signal (ag-correction-s-signal operation)
				 :step-kernel (get 'X 'kernel)))

(defmethod change-node-content ((operation ag-Z-correction))
  (make-cnc-correction-operation :qubit (ag-correction-qubit operation)
				 :signal (ag-correction-s-signal operation)
				 :step-kernel (get 'Z 'kernel)))


;;;; INSTANTIATE-NODE-CONTENT: second pass, fills in the dependent data (e.g. names of input and output nodes)

(defgeneric instantiate-node-content (cnc-obj node cnc-graph))

(defmethod instantiate-node-content :before ((operation cnc-operation) node cnc-graph)
  (let ((input-cnc-tangle  (get-cnc-tangle (node-upstream-nodes node)))
	(output-cnc-tangle (get-cnc-tangle (node-downstream-nodes node)))
	(signals           (get-cnc-signals (node-upstream-nodes node))))
    (with-slots (input-item-collection
		 output-item-collection
		 input-tag-collection
		 output-tag-collection
		 signal-item-collection
		 step-kernel
		 qubit
		 qubit-index
		 size)
	operation
      (assert input-cnc-tangle) (assert output-cnc-tangle) ;signals can be empty
      (setf qubit-index (when qubit (qubit-tensor-index qubit (cnc-tangle-tangle input-cnc-tangle)))
            input-item-collection (cnc-tangle-item-collection input-cnc-tangle)
	    input-tag-collection (cnc-tangle-tag-collection input-cnc-tangle)
	    output-item-collection (cnc-tangle-item-collection output-cnc-tangle)
	    output-tag-collection (cnc-tangle-tag-collection output-cnc-tangle)
	    signal-item-collection (when signals (cnc-signal-item-collection signals))
	    size (cnc-tangle-size input-cnc-tangle)))))

;;; stub, non-operation nodes do nothing
(defmethod instantiate-node-content (content node cnc-graph))

(defmethod instantiate-node-content ((operation cnc-operation) node cnc-graph)
  (with-slots (step-kernel input-item-collection output-item-collection output-tag-collection signal-item-collection qubit-index size)
      operation
    (setf step-kernel
	  (apply-kernel step-kernel
					; input: tangle
			(list (item-collection-name input-item-collection))  
					; output: tangle, tag
			(list (item-collection-name output-item-collection) 
			      (tag-collection-name output-tag-collection))
					; params: size qid
			(mapcar #'number-to-string (list size qubit-index))))))

(defmethod instantiate-node-content ((operation cnc-entanglement-operation) node cnc-graph)
  (with-slots (step-kernel input-item-collection output-item-collection output-tag-collection 
	       signal-item-collection  size qubit-2 qubit-index qubit-2-index 
	       tuner-name tuner-args)
      operation
    (setf qubit-2-index (qubit-tensor-index qubit-2 (cnc-tangle-tangle (get-cnc-tangle (node-upstream-nodes node)))))
    (let* ((qi-1 (max qubit-index qubit-2-index))
	   (qi-2 (min qubit-index qubit-2-index))
	   (params (mapcar #'number-to-string (list size qi-1 qi-2))))
      (setf step-kernel (apply-kernel step-kernel
					; input: tangle
				      (list (item-collection-name input-item-collection))
					; output: tangle, tag
				      (list (item-collection-name output-item-collection) 
					    (tag-collection-name output-tag-collection))
					; params: size qid qid-2
				      params)
	   ;; tuner-name "e_tuner"
	   ;; tuner-args (append params (list (item-collection-name input-item-collection)))
	    ))))

(defmethod instantiate-node-content ((operation cnc-kronecker-operation) node cnc-graph)
  (with-slots (step-kernel input-item-collection input-tag-collection
			   input2-item-collection input2-tag-collection 
			   output-item-collection output-tag-collection 
			   size size-2
			   tuner-name tuner-args)
      operation
    (assert (= (list-length (get-cnc-tangles (node-upstream-nodes node)))
	       2))
    (let ((cnc-tangle-2 (second (get-cnc-tangles (node-upstream-nodes node)))))
      (setf input2-item-collection (cnc-tangle-item-collection cnc-tangle-2)
	    input2-tag-collection  (cnc-tangle-tag-collection cnc-tangle-2)
	    size-2 (cnc-tangle-size cnc-tangle-2))
      ;; (when (> size-2 size)
      ;; 	;; swap out tangle-1 and tangle-2 such that tangle-1 becomes the largest
      ;; 	;; ... dirty dirty hack to limit the number of suspended item
      ;; 	(rotatef input2-tag-collection input-tag-collection)
      ;; 	(rotatef input2-item-collection input-item-collection)
      ;; 	(rotatef size-2 size))
      (setf step-kernel (apply-kernel step-kernel
					;  (tangle-1 tangle-2) (tangle-out tag-out) (size size-2)
				      (list (item-collection-name input-item-collection)
					    (item-collection-name input2-item-collection))
				      (list (item-collection-name output-item-collection)
					    (tag-collection-name  output-tag-collection))
				      (mapcar #'number-to-string (list size-2)))
	    tuner-name "kron_tuner"
	    tuner-args (list (number-to-string size-2)
			     (item-collection-name input2-item-collection))))))

(defmethod instantiate-node-content ((operation cnc-measurement-operation) node cnc-graph)
  ;; the operation step kernel performs the M operation, but there is a second step 'check-M' that needs to be introduced
  (with-slots (check-step-kernel step-kernel input-item-collection output-item-collection output-tag-collection 
	       signal-item-collection qubit-index size do-m-tag-collection
	       tuner-name tuner-args)
      operation
    (setf check-step-kernel
					; (in_tangle signals) 
					; (do_m_tags out_tangle out_tags signals) 
					; (qid)
	  (apply-kernel check-step-kernel
					; input: tangle,signals
			(list (item-collection-name input-item-collection)
			      (item-collection-name signal-item-collection))
					; output: tag, tangle, tag, signals
			(list (tag-collection-name do-m-tag-collection)
			      (item-collection-name output-item-collection)
			      (tag-collection-name output-tag-collection)
			      (item-collection-name signal-item-collection))
					; params: qid
			(mapcar #'number-to-string (list qubit-index))))    
    ;; this will apply the M operation kernel
    ;;   (in_tangle) (out_tangle out_tags) (size qid)
    (setf step-kernel
	  (apply-kernel step-kernel
					; input: tangle
			(list (item-collection-name input-item-collection))  
					; output: tangle, tag
			(list (item-collection-name output-item-collection) 
			      (tag-collection-name output-tag-collection))
					; params: size qid
			(mapcar #'number-to-string (list size qubit-index))))
    (setf tuner-name "m_tuner"
	  tuner-args (list (number-to-string size)
			   (number-to-string qubit-index)
			   (item-collection-name input-item-collection)))))

(defmethod instantiate-node-content ((cnc-tangle cnc-tangle) node cnc-graph)
  ;; if tangle is an input or output node, add a cnc-step to it
  (cond ((input-node-p node cnc-graph)
	 (setf (node-content node)
	       (make-io-cnc-tangle cnc-tangle :input)))
	((output-node-p node cnc-graph)
	 (setf (node-content node)
	       (make-io-cnc-tangle cnc-tangle :output)))))

;;;; MAKE-IO-CNC-TANGLE

(defgeneric make-io-cnc-tangle (tangle direction))

(defmethod make-io-cnc-tangle (cnc-tangle (direction (eql :input)))
  (with-slots (tangle
	       size
	       item-collection
	       tag-collection)
      cnc-tangle
    (make-cnc-input-tangle :tangle tangle
			   :size size
			   :item-collection item-collection
			   :tag-collection tag-collection
			   :generator-tag-collection (make-tag-collection :name (symbol-name (gensym "do_generate_")))
			   :step-kernel (apply-kernel (get 'gen_tangle 'kernel)
						      () 
						      (list (item-collection-name item-collection)
							    (tag-collection-name tag-collection))
						      (list (number-to-string size))))))

(defmethod make-io-cnc-tangle (cnc-tangle (direction (eql :output)))
  (with-slots (tangle
	       size
	       item-collection
	       tag-collection)
      cnc-tangle
    (make-cnc-output-tangle :tangle tangle
			    :size size
			    :item-collection item-collection
			    :tag-collection tag-collection
			    :step-kernel (apply-kernel (get 'output_tangle 'kernel)
						       (list (item-collection-name item-collection))
						       ()
						       (list (number-to-string size))))))

;; (defun make-tensoring-entanglement-operation (cnc-entanglement-operation input-cnc-tangles)
;;   (with-slots (input-item-collection output-item-collection input-tag-collection output-tag-collection step-kernel size qubit qubit-2)
;;       cnc-entanglement-operation
;;     (let* ((input-tangle-2         (first (remove input-item-collection input-cnc-tangles
;; 						  :key #'cnc-tangle-item-collection)))
;; 	   (input2-item-collection (cnc-tangle-item-collection input-tangle-2))
;; 	   (input2-tag-collection  (cnc-tangle-tag-collection input-tangle-2))
;; 	   (size-2                 (cnc-tangle-size input-tangle-2))
;; 	   (tensor-step-kernel     (get 'kronecker    'kernel))
;; 	   (tensor-tag-collection  (make-tag-collection :name (symbol-to-string (gensym "tag-cross-pairs")) 
;; 							:tag-type "std::pair<unsigned int,unsigned int>"))
;; 	   (tensor-item-collection (make-item-collection :name (symbol-to-string (gensym "tensored_")))))
;;       (make-cnc-tensoring-entanglement-operation :input-item-collection  input-item-collection
;; 						 :output-item-collection output-item-collection
;; 						 :input-tag-collection   input-tag-collection
;; 						 :output-tag-collection  output-tag-collection
;; 						 :step-kernel            step-kernel
;; 						 :qubit                  qubit
;; 						 :size                   size
;; 						 :size-2                 size-2
;; 						 :qubit-2                qubit-2
;; 						 :input2-item-collection input2-item-collection
;; 						 :input2-tag-collection  input2-tag-collection
;; 						 :tensor-tag-collection  tensor-tag-collection
;; 						 :tensor-item-collection tensor-item-collection
;; 						 :tensor-step-kernel     tensor-step-kernel))))

(defun construct-cnc-program-from-graph (cnc-graph)
  (loop
     for node in (graph-nodes cnc-graph)
     for node-content = (node-content node)
     for node-prescriptions = (cnc-node-prescriptions node-content node)
     appending node-prescriptions into prescriptions
 ;; currently adding a single [int signals <bool>] collection in the
    ;; generator itself when (cnc-operation-p (node-content node)) do
    ;; (pushnew (cnc-operation-signal-item-collection (node-content
    ;; node)) items)
    
     when (cnc-tangle-p node-content)
       collect (cnc-tangle-item-collection node-content) 
         into items
     when (cnc-input-tangle-p node-content)
       collect (cnc-input-tangle-generator-tag-collection node-content)
         into generator-tag-collections
     when (cnc-kronecker-operation-p node-content)
       collect (tag-collection-name (cnc-kronecker-operation-input2-tag-collection node-content))
         into dangling-tag-names
     ;; adding tuners
     when (and (cnc-operation-p node-content)
	       (cnc-operation-tuner-name node-content))
     collect (cons (symbol-name (cnc-step-name (cnc-operation-step-kernel node-content)))
		   (cons  (cnc-operation-tuner-name node-content)
			  (cnc-operation-tuner-args node-content)))
       into tuned-steps
     finally
       (let* ((steps       (mapcar #'car prescriptions))
	      (tag-names   (mapcar (compose #'tag-collection-name #'cdr) prescriptions))
	      (step-names  (mapcar (compose #'symbol-name #'cnc-step-name) steps))
	      (step-bodies (mapcar #'cnc-step-body steps)))
	 (return (make-cnc-program :items         (mapcar #'item-collection-name items)
				   :tags          (append dangling-tag-names tag-names)
				   :step-names    step-names
				   :step-bodies   step-bodies
				   :input-tags    (mapcar #'tag-collection-name generator-tag-collections)
				   :prescriptions (pairlis step-names tag-names)
				   :tuned-steps   tuned-steps)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  CNC STEP FUNCTIONS  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct cnc-step
  (name (gensym "step"))
  body
  inputs
  outputs)

(defstruct kernel 
  name
  parameter-names
  body-generator)

(defun apply-kernel (kernel inputs outputs &optional parameter-values)
  (assert (same-length-p parameter-values (kernel-parameter-names kernel)))
  (make-cnc-step :name (gensym (kernel-name kernel))
		 :body (funcall (kernel-body-generator kernel)
				inputs
				outputs
				parameter-values)
		 :inputs inputs
		 :outputs outputs))

(defmacro defkernel (name (&rest inputs) (&rest outputs) (&rest parameters)
		     body-string)
  (alexandria:with-gensyms (g-name g-inputs g-outputs g-parameters g-body-string)
    `(let  ((,g-name ,(symbol-to-string name))
	    (,g-inputs (list ,@(symbol-list-to-string inputs)))
	    (,g-outputs (list ,@(symbol-list-to-string outputs)))
	    (,g-parameters (list ,@(symbol-list-to-string parameters)))
	    (,g-body-string ,body-string))
       (flet ((body-generator (actual-inputs actual-outputs parameters)
		(assert (and (same-length-p actual-inputs ,g-inputs)
			     (same-length-p actual-outputs ,g-outputs)
			     (same-length-p parameters ,g-parameters)))
		(replace-all-matching-pairs (pairlis (mapcar #'concatenate-escapes (append ,g-inputs ,g-outputs ,g-parameters))
						     (append actual-inputs actual-outputs parameters))
					    ,g-body-string)))
	 (setf (get ',name 'kernel)
	       (make-kernel :name ,g-name
			    :parameter-names ,g-parameters
			    :body-generator #'body-generator))))))

;; (defgeneric instantiate-kernel (step-kernel input-cnc-tangle output-cnc-tangle signals &rest parameters))

;; ;;; default
;; (defmethod instantiate-kernel (step-kernel)
;; 			       input-cnc-tangle
;; 			       output-cnc-tangle
;; 			       signals
;; 			       &rest parameters)
;;   (apply-kernel step-kernel
;; 		(list (cnc-tangle-item-collection input-cnc-tangle))
;; 		(list (cnc-tangle-item-collection output-cnc-tangle)
;; 		      (cnc-tangle-tag-collection  output-cnc-tangle))
;; 		(mapcar #'number-to-string parameters))


(defkernel E (in_items) (out_items out_tags) (size qid_1 qid_2)
"
amplitude a_i;
const int i = t;
if (`qid_2` > `qid_1`) { printf(\"WARNING: qid2 > qid1\\n\"); }
c.`in_items`.get( i , a_i );

const int f_i = tensor_permute( i , `size` , `qid_1`, `qid_2` );

if (f_i % 4 == 3)
  a_i = - a_i;

c.`out_items`.put( i , a_i, 1 );
c.`out_tags`.put( i );
")

(defkernel X (in_tangle) (out_tangle out_tags) (size qid)
"
amplitude a_i;
const int i = t;
const int m = `qid`;
const int n = `size` / `qid`;

c.`in_tangle`.get( i , a_i );

const int f_i = permute( i , n, m );

const int f_target_index = f_i ^ 1;
const int target_index = permute( f_target_index , m , n );

c.`out_tangle`.put( target_index , a_i , 1 );
c.`out_tags`.put( target_index );
")

(defkernel Z (in_tangle) (out_tangle out_tags) (size qid)
"
amplitude a_i;
const int i = t;
const int m = `qid`;
const int n = `size` / `qid`;

c.`in_tangle`.get( i , a_i );

const int f_i = permute( i , n, m );

if (f_i % 2)
  a_i = -a_i;

c.`out_tangle`.put( i , a_i , 1);
c.`out_tags`.put( i );
")

(defkernel M (in_tangle) (out_tangle out_tags) (size qid)
"
amplitude a_i1;
amplitude a_i2;
const unsigned int i = t;
const unsigned int m = `qid`;
//const unsigned int n = `size` / `qid`;
//const unsigned int f_i = permute( i , m , n );
//const double pi = 3.14159265;
const double alpha = 0;  // alpha is normally a param, constant for now
const amplitude phi_0 = sqrt(2);
const amplitude phi_1 = exp(alpha) * sqrt(2); // normally should be -ia (complex)

if ((i & m) == 0) {
  const unsigned int i2 = i + m;
  c.`in_tangle`.get( i  , a_i1 );
  c.`in_tangle`.get( i2 , a_i2 );
  const amplitude new_amp = a_i1 * phi_0 + a_i2 * phi_1;
  const unsigned new_index = compact_but_index(i,m);
  c.`out_tangle`.put( new_index , new_amp , 1);
  c.`out_tags`.put( new_index );
}
")

#|
if ((f_i % 2) == 0) {
  const unsigned int i2 = permute( f_i^1 , m , n );
  const unsigned int new_index = permute( f_i/2 , m , n );
  // phi's will depend on the angle of measurement
  const amplitude phi_1 = 0;
  const amplitude phi_2 = 1;
  c.`in_tangle`.get( i  , a_i1 );
  c.`in_tangle`.get( i2 , a_i2 );
  // normally apply factor to i1 and i2 here, skipping this for now 
  c.`out_tangle`.put( new_index , a_i1 * phi_1 + a_i2 * phi_2 , 1);
  c.`out_tags`.put( new_index );
}
|#

;(defkernel merge_M (in_tangle) (out_tangle out_tags) (size qid))

(defkernel check_M (in_tangle signals) (do_m_tags out_tangle out_tags signals) (qid)
"
amplitude amp;
  // shortcut: lets say measurement always returns 1;
bool measurement_result = true;
if (measurement_result) {
  c.`signals`.put(t,true);
  c.`do_m_tags`.put(t); // lets do the measurement
}
else {
  c.`in_tangle`.get(t,amp); // proceed with the next computation
  c.`signals`.put(t,false);
  c.`out_tangle`.put( t , amp , 1 );
  c.`out_tags`.put(t);
}
"
)

(defkernel gen_tangle () (tangle tags) (size)
"
const amplitude amp( 1.0/`size` );
for(int i(0);i<`size`;++i) {
  c.`tangle`.put( i , amp , 1 );
  c.`tags`.put(i);
}
")

(defkernel output_tangle (tangle) () (size)
"
amplitude amp;
c.`tangle`.get(t,amp);
printf(\"`tangle` %d: %1.4f\\n\",t,amp);
")

(defkernel kron (tangle-1 tangle-2) (tangle-out tag-out) (size-2)
"
amplitude amp_1;
amplitude amps[`size-2`];

c.`tangle-1`.get(t,amp_1);
for(int i(0);i<`size-2`;++i) {
  c.`tangle-2`.get(i,amps[i]);
}
for(int i(0);i<`size-2`;++i) {
  const unsigned int new_index( t * `size-2` + i );
  c.`tangle-out`.put( new_index, amp_1 * amps[i] , 1);
  c.`tag-out`.put(new_index);
}
")

;; (pprint (apply-kernel (get 'E 'kernel)
;; 	       '("foo")
;; 	       '("bar" "baz")
;; 	       (mapcar #'number-to-string
;; 		       '(8 2 4))))

;; (pprint (apply-kernel (get 'X 'kernel)
;; 	       '("foo")
;; 	       '("bar" "baz")
;; 	       (mapcar #'(lambda (num) (format nil "~A" num)) '(8 2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  STRING/MATCH OPERATIONS  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun replace-matching (match-string target-string replace-by-string)
  (declare (type string match-string target-string replace-by-string))
  (let ((index (search match-string target-string)))
    (if index
	(with-output-to-string (out) 
	  (write-string target-string out :end index)
	  (write-string replace-by-string out)
	  (write-string target-string out :start (+ index (length match-string)))))))

(defun replace-all-matching (match-string target-string replace-by-string)
  (let ((replaced (replace-matching match-string target-string replace-by-string)))
    (if replaced
	(replace-all-matching match-string replaced replace-by-string)
	target-string)))

(defun replace-all-matching-pairs (match-replace-pairs target-string)
  (loop 
     for (match . replace) in match-replace-pairs
     for replaced = (replace-all-matching match target-string replace)
     then (replace-all-matching match replaced replace)
     finally (return replaced)))

(defun number-to-string (num)
  (format nil "~A" num))

(defun concatenate-escapes (str)
  (concatenate 'string "`" str "`"))

;;;; MISC UTILITY FUNCTIONS

(defun same-length-p (list-1 list-2)
  (= (list-length list-1)
     (list-length list-2)))



;;TEST

(defstruct cnc-program 
  items
  tags
  step-names
  step-bodies
  input-tags
  prescriptions
  tuned-steps)

#+nil(let* ((g (mc-graph-to-cnc-graph (compile-mc '((E 1 2) (M 1 0) (X 2 (q 1))))))
       (cnc-program (construct-cnc-program-from-graph g))  ; 'flatten' graph
       )
;  (pprint cnc-program)
  (with-slots (items tags step-names step-bodies input-tags prescriptions tuned-steps)
      cnc-program
    (cnc-gen:build items tags step-names step-bodies input-tags prescriptions tuned-steps)))

#+nil(let* ((g (mc-graph-to-cnc-graph (compile-mc '((E 1 2) (M 1 0)))))
       (cnc-program (construct-cnc-program-from-graph g))  ; 'flatten' graph
       )
;  (pprint cnc-program)
  (with-slots (items tags step-names step-bodies input-tags prescriptions tuned-steps)
      cnc-program
    (cnc-gen:build items tags step-names step-bodies input-tags prescriptions tuned-steps)))

#+nil(let* ((g (mc-graph-to-cnc-graph (compile-mc *short-w3-program*)))
       (cnc-program (construct-cnc-program-from-graph g))  ; 'flatten' graph
       )
;  (pprint cnc-program)
  (with-slots (items tags step-names step-bodies input-tags prescriptions tuned-steps)
      cnc-program
    (cnc-gen:build items tags step-names step-bodies input-tags prescriptions tuned-steps)))



#+nil(let* ((g (mc-graph-to-cnc-graph (compile-mc *w3-program*)))
       (cnc-program (construct-cnc-program-from-graph g))  ; 'flatten' graph
       )
;  (pprint cnc-program)
  (with-slots (items tags step-names step-bodies input-tags prescriptions tuned-steps)
      cnc-program
    (cnc-gen:build items tags step-names step-bodies input-tags prescriptions tuned-steps)))

;(show-dot (compile-mc *w3-program*))

#+nil(let* ((g (mc-graph-to-cnc-graph (compile-mc '((E 0 1) (E 1 2) (E 3 4) (E 9 10) (E 11 12) (M 0 0) (M 1 0) (X 2 (q 1))))))
       (cnc-program (construct-cnc-program-from-graph g))  ; 'flatten' graph
       )
;  (pprint cnc-program)
  (with-slots (items tags step-names step-bodies input-tags prescriptions tuned-steps)
      cnc-program
    (cnc-gen:build items tags step-names step-bodies input-tags prescriptions tuned-steps)))

#+nil(let* ((g (mc-graph-to-cnc-graph (compile-mc '((E 1 2) (E 2 3)))))
       (cnc-program (construct-cnc-program-from-graph g))  ; 'flatten' graph
       )
;  (pprint cnc-program)
  (with-slots (items tags step-names step-bodies input-tags prescriptions)
      cnc-program
    (cnc-gen:build items tags step-names step-bodies input-tags prescriptions)))

(defun mc-read-compile ()
  (sb-sys:enable-interrupt sb-unix:sigint #'(lambda () (sb-ext:quit)))
  (format t " mcc> ")
  (finish-output)
  (let ((mc-program (read *standard-input* nil)))
    (format t "Generating MC program graph... ")
    (let ((mc-graph (compile-mc mc-program)))
      (format t "done~%Generating CnC-specific graph... ")
      (let ((g (mc-graph-to-cnc-graph mc-graph)))
	(format t "done~%Collecting data for CnC code generation... ")
	(let ((cnc-program (construct-cnc-program-from-graph g)))
	  (with-slots (items tags step-names step-bodies input-tags prescriptions tuned-steps)
	      cnc-program
	    (format t "done~%Beginning code generation.~%")
	    (cnc-gen:build items tags step-names step-bodies input-tags prescriptions tuned-steps)
	    'ok))))))