;;;; Copyright (C) 2011 by Yves Vandriessche

;;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;;; of this software and associated documentation files (the "Software"), to deal
;;;; in the Software without restriction, including without limitation the rights
;;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the Software is
;;;; furnished to do so, subject to the following conditions:

;;;; The above copyright notice and this permission notice shall be included in
;;;; all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;;; THE SOFTWARE.


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
;;;; 4. Compiler Interface

#+nil(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload 'alexandria)
  (declaim (optimize (speed 0) (debug 3) (safety 3))))

(defpackage :mc-graph
  (:nicknames :mcg)
  (:use :cl)
  (:import-from alexandria with-gensyms if-let compose)
  (:export compile-mc
	   graph make-graph graph-nodes graph-input-nodes graph-output-nodes
	   node make-node node-label node-content node-upstream-nodes
	   node-downstream-nodes 
	   input-node-p output-node-p
	   add-node add-input-node add-output-node))

(eval-when (:compile-toplevel :load-toplevel)
  (declaim (optimize (speed 0) (debug 3) (safety 3))))

(in-package :mcg)

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

(defconstant +parse-package+ #.(find-package :mcg))
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

(defstruct operation)

(defstruct (ag-operation (:include operation)))

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

#+nil(defun node-sibling (node)
  (let ((children (node-downstream-nodes node)))
    (assert (= 1 (length children)))
    (let ((siblings (node-upstream-nodes (first children))))
      (assert (= 1 (length siblings)))
      (first siblings))))



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

(defun tangle-size (tangle)
  (declare (type tangle tangle))
  "Returns the number of amplitudes needed to represent this tangle,
power of two of the tangle size"
  (expt 2 (length (tangle-qubits tangle))))

;;; 3.2.2 Signal Map

(defstruct signal-map
  input-signals
  qubit-signals)

(defun make-empty-signal-map () 
  (make-signal-map))

;;; 3.2.3 Operation-specific manipulators

;; Datastructures (see also ag-operations)

(defstruct (kronecker-operation (:include operation))
  qubit-1
  qubit-2)

(defgeneric qubit-dependencies (operation))
(defmethod qubit-dependencies ((operation ag-correction))
  (list (ag-correction-qubit operation)))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  4. COMPILER INTERFACE   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;;;; PRINT GRAPH TO GRAPHVIZ ;;;;

(defun dot-node-label (node graph)
  (declare (ignore graph))
  (let ((content (node-content node)))
    (typecase content
      (mcg::tangle (format nil "Tangle { ~{~S ~}}" 
		      (mcg::tangle-qubits content)))
      (mcg::ag-entanglement 
       (format nil "Entangle (~A, ~A)"
	       (mcg::ag-entanglement-qubit-1 content)
	       (mcg::ag-entanglement-qubit-2 content)))
      (mcg::ag-measurement 
       (format nil "Measure ~A" 
	       (mcg::ag-measurement-qubit content)))
      (mcg::ag-X-correction (format nil "X ~A" (mcg::ag-correction-qubit content)))
      (mcg::ag-Z-correction (format nil "Z ~A" (mcg::ag-correction-qubit content)))
      (mcg::kronecker-operation (format nil "Kronecker Product"))
      (otherwise (node-label node)))))

(defun dot-node-shape (node graph)
  (typecase (node-content node)
    ((or mcg::ag-operation mcg::operation) "ellipse")
    ((or mcg::tangle) "rectangle")
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
  (format t "Dumped dot file in /tmp/graph.dot~%")
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


