
;;;; File Organisation:
;;;; 1. Abstract Grammar

(eval-when (:compile-toplevel :load-toplevel)
 (declaim (optimize (speed 0) (debug 3) (safety 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 1.  ABSTRACT GRAMMAR    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Read-time constants

(defmacro define-constant (name value &optional doc)
  "Allows the definition of symbol lists as constants without the compiler complaining"
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

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
		  (error "Invalid measurment angle: ~A~%" angle))))
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
  (let ((operation-name (intern (symbol-name (car exp)))))
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
doing the same with the other stages. Existing connections are
overwritten."
  (when pipeline-stages
    (let ((upstream-stage (first pipeline-stages)) 
	  (downstream-pipeline-stages (rest pipeline-stages)))
      (assert upstream-stage (upstream-stage) "Trying to connect nil into a pipeline, was expecting a list of nodes")
      (when downstream-pipeline-stages
	(let ((downstream-stage (first downstream-pipeline-stages)))
	  (loop for node in upstream-stage
	     do (setf (node-downstream-nodes node) downstream-stage))
	  (loop for node in downstream-stage
	     do (setf (node-upstream-nodes node) upstream-stage))
	  (pipeline-connect downstream-pipeline-stages))))))

(defun pipeline-to-graph (pipeline-stages)
  (pipeline-connect pipeline-stages)
  (make-graph :nodes (loop for nodes in pipeline-stages
			  appending nodes)
	      :input-nodes (first pipeline-stages)
	      :output-nodes (first (last pipeline-stages))))


;;; 3.2 Compiler Datastructures

;;; 3.2.1 Tangle

(defstruct tangle
  "Simple list of qubit identifiers"
  qubits)

(defun merge-tangles (tangle-1 tangle-2)
  "Returns a new tangle containing the qubits of both tangles, in order."
  (make-tangle :qubits (union (tangle-qubits tangle-1)
			      (tangle-qubits tangle-2))))

(defun find-qubit (tangle qubit)
  "Returns nil if the qubit could not be found in the tangle, non-nil otherwise"
  (declare (type tangle tangle))
  (declare (type #.+qubit-id-type+ qubit))
  (find qubit (tangle-qubits tangle)))

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
  )

(defgeneric input-signal-dependencies (operation))
(defmethod input-signal-dependencies ((operation ag-measurement))
  )

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

;; Operation-specific program graph code

(defgeneric make-operation-graph (operation input-nodes))
(defmethod make-operation-graph ((operation ag-entanglement) input-nodes)
  (let* ((output-node (make-output-node operation input-nodes))
	 (entangle-node (make-computation-node operation)))
    (case (length input-nodes)
      (1 (pipeline-to-graph (pipeline (stage input-nodes)
				      (stage entangle-node)
				      (stage output-node))))
      (2 (let ((kron-operation-graph (make-operation-graph (make-kronecker-operation :qubit-1 (ag-entanglement-qubit-1 operation)
										     :qubit-2 (ag-entanglement-qubit-2 operation)) 
							   input-nodes)))
	   (merge-program-graphs kron-operation-graph
				 (make-operation-graph operation
						       (graph-output-nodes kron-operation-graph)))))
      (otherwise (error "Entanglement operation got more than 2 input nodes, this shouldn't happen.")))))

(defmethod make-operation-graph ((operation kronecker-operation) input-nodes)
  (pipeline-to-graph (pipeline (stage input-nodes)
			       (stage (make-computation-node operation)) 
			       (stage (make-output-node operation input-nodes)))))

(defmethod make-operation-graph ((operation ag-measurement) input-nodes)
  )

(defmethod make-operation-graph ((operation ag-correction) input-nodes)
  )


(defgeneric make-computation-node (operation))
(defmethod make-computation-node ((operation ag-entanglement))
  (make-node :label (gensym "OP-Entangle-")
	     :content operation))

(defmethod make-computation-node ((operation kronecker-operation))
  (make-node :label (gensym "OP-Kron-")
	     :content operation))


(defgeneric make-data-node (obj))
(defmethod make-data-node ((tangle tangle))
  (make-node :label (gensym "DATA-Tangle-")
	     :content tangle))

(defmethod make-data-node ((signals signal-map))
  (make-node :label (gensym "Signals-")
	     :content signals))


(defgeneric make-output-node (operation input-nodes))
(defmethod make-output-node ((operation ag-entanglement) input-nodes)
  (let ((input-tangles (mapcar #'node-content input-nodes)))
    (case (length input-nodes)
      (1 (make-data-node (first input-tangles)))
      (2 (make-data-node (merge-tangles (first input-tangles)
					  (second input-tangles)))))))

(defmethod make-output-node ((operation kronecker-operation) input-nodes)
  (assert (= (length input-nodes) 2))
  (make-data-node (merge-tangles (node-content (first input-nodes))
				   (node-content (second input-nodes)))))


;; Searching in Graph and Node contents

(defgeneric find-input-nodes (operation program-graph))
(defmethod find-input-nodes ((operation ag-measurement) program-graph)
  "Returns a list with a tangle node and a signal node (in that
order).  A new tangle or signal node is created when it cannot find
either in the program graph."
  (let ((tangle-node  (find-tangle-node program-graph (ag-measurement-qubit operation)))
	(signal-map-node (find-signal-map-node program-graph
					       (qubit-signal-dependencies operation)
					       (input-signal-dependencies operation))))
    (list (ensure-tangle-node tangle-node operation)
	  (ensure-signal-map-node signal-map-node operation))))

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
	  ((or tangle-node-1 tangle-node-2)  ; one is nil
	   (list (ensure-tangle-node tangle-node-1 operation)
		 (ensure-tangle-node tangle-node-2 operation)))
	  (t (error "find-input-nodes failed to find or create the correct tangles")))))


(defgeneric find-input-nodes (operation program-graph)
  (:documentation "Returns the relevant input-nodes for a certain
  operation (in abstract grammar form). Creates new nodes when
  necessary"))

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

(defun find-tangle-node (graph qubit)
  (find-output-node graph 
		    #'(lambda (content)
			(and (tangle-p content)		   
			     (find-qubit content qubit)))))

(defun find-signal-map-node (graph qubit-signal-dependencies input-signal-dependencies)
  "Returns a signal-map object that contains all the signal dependencies, nil otherwise."
  (find-output-node graph
		    #'(lambda (content)
			(and (signal-map-p content)
			     (subsetp qubit-signal-dependencies
				      (signal-map-qubit-signals content))
			     (subsetp input-signal-dependencies
				      (signal-map-input-signals content))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   COMPILER INTERFACE   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-to-graph (sequence)
  (loop for operation in sequence
     with program-graph = (make-empty-graph)
     do (setf program-graph (compile-operation operation program-graph))
     finally (return program-graph)))

(defun compile-operation (operation program-graph)
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
  (make-operation-graph (make-ag-entanglement :qubit-1 2 :qubit-2 1) (list (make-data-node (make-tangle (list 2))) 
									   (make-data-node (make-tangle (list 1 3))))))


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

;(show-dot (compile-to-graph (parse-sequence '((E 1 2) (E 5 6) (E 3 4) (E 1 5) (E 6 3)))))

;(test-all)