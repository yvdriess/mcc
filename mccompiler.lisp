
(defpackage :mc-compiler
  (:nicknames :mcc)
  (:use :cl :mcg :cnc)
    (:import-from alexandria if-let)
  (:export compile-to-cnc))

(in-package :mcc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; these get used at macro-expand time
  (defun symbol-to-string (sym)
    (string-downcase (symbol-name sym)))
  (defun symbol-list-to-string (symlist)
    (mapcar #'symbol-to-string symlist))
  )


(defvar *kron-depends-body*
  "
for( int t(0) ; t < size_2 ; ++i ) {
  dC.depends( in_items , t );
}
")
(defvar *M-depends-body*
   "
if ( (t & qid) == 0 ) {
  dC.depends( in_tangle , t );
  dC.depends( in_tangle , t + qid);
}"
   )

(defkernel E ((:consumes in_items) 
	      (:produces out_items) 
	      (:controls out_tags)
	      (:parameters (size int) 
			   (qid_1 int) 
			   (qid_2 int)))
  "
amplitude a_i;
if (qid_2 > qid_1) { printf(\"WARNING: qid2 > qid1\\n\"); }
in_items.get( i , a_i );

const int f_i = tensor_permute( i , size , qid_1, qid_2 );

if (f_i % 4 == 3)
  a_i = - a_i;

out_items.put( i , a_i );
out_tags.put( i );
")

(defkernel kron ((:consumes tangle_1 tangle_2)
		 (:produces tangle_out)
		 (:controls tag_out)
		 (:parameters (size_2 int))
		 (:depends ((tangle_1 tangle_items)
			    (tangle_2 tangle_items)
			    (size_2 int))
			   *kron-depends-body*))
  "
amplitude amp_1;
amplitude amps[size_2];

tangle_1.get(t,amp_1);
for(int i(0);i<size_2;++i) {
  tangle_2.get(i,amps[i]);
}
for(int i(0);i<size_2;++i) {
  const unsigned int new_index( t * size_2 + i );
  tangle_out.put( new_index, amp_1 * amps[i] );
  tag_out.put(new_index);
}
")

(defkernel M ((:consumes in_tangle) 
	      (:produces out_tangle) 
	      (:controls out_tags)
	      (:parameters (size int) 
			   (qid int)
			   (angle double))
	      (:depends ((in_tangle tangle_items)
			 (qid int))
			*M-depends-body*))
  "
const unsigned int i = t;

if ( (i & qid) == 0 ) {
  amplitude a_i1;
  amplitude a_i2;
  const amplitude phi_1 = std::exp(amplitude(0,-angle));
  const unsigned int i2 = i + qid;
  in_tangle.get( i  , a_i1 );
  in_tangle.get( i2 , a_i2 );
  const amplitude new_amp = a_i1 - a_i2 * phi_1;
  const unsigned new_index = compact_bit_index(i,qid);
  out_tangle.put( new_index , new_amp );
  out_tags.put( new_index );
}
")


(defkernel X ((:consumes in_tangle) 
	      (:produces out_tangle) 
	      (:controls out_tags)
	      (:parameters (size int)
			   (qid int)))
  "
amplitude a_i;
const int i = t;
const int m = qid;
const int n = size / qid;
bool signal=true;

in_tangle.get( i , a_i );

const int target_index = 
  signal ? permute( permute(i, n, m)^1, m, n )
         : i;

out_tangle.put( target_index , a_i );
out_tags.put( target_index );
")

(defkernel Z ((:consumes in_tangle) 
	      (:produces out_tangle) 
	      (:controls out_tags)
	      (:parameters (size int)
			   (qid int)))
"
amplitude a_i;
const int i = t;
const int m = qid;
const int n = size / qid;
bool signal=true;

in_tangle.get( i , a_i );

const int f_i = permute( i , n, m );

if( signal )
  if (f_i % 2)
    a_i = -a_i;

out_tangle.put( i , a_i );
out_tags.put( i );
")

(defkernel source ((:produces out_items)
		   (:contols out_tag)
		   (:parameters (size int)))
"
for( int i(0) ; i < size ; ++i ) {
  out_items.put(i, 1/sqrt(size));
  out_tag.put(i);
}
")

(defkernel sink ((:consumes in_items)
		 (:parameters (size int)))
"
static int n(0);
printf(\"tangle %d:\\n\", n++);
for( int i(0) ; i < size ; ++i ) {
  amplitude amp;
  in_items.get(i, &amp);
  printf(\"  [%d]: (%1.4f,%1.4f) \\n\", i, amp.real(), amp.imag());
}
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 1. MC GRAPH TO CNC GRAPH  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mc-graph-to-cnc-graph (mc-graph)
  (let ((cnc-graph (mcg::make-graph))
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

(defun expand-to-cnc-node (node)
  (declare (type mcg::node node))
  (let ((new-node (make-node :content (change-node-content (node-content node))
			     :upstream-nodes (node-upstream-nodes node)
			     :downstream-nodes (node-downstream-nodes node))))
    new-node))

					;(pprint (mc-graph-to-cnc-graph (compile-mc '((E 1 2) (M 1 0) (X 2 (q 1))))))

;;;; COARSE MCC AS CNC GRAPH MODEL:  TANGLE, SIGNAL, OPERATIONS

(defstruct cnc-tangle
  tangle
  size
  name)

(defstruct (cnc-input-tangle (:include cnc-tangle))
)

(defstruct (cnc-output-tangle (:include cnc-tangle))
)

(defstruct cnc-signal
  name)

(defstruct cnc-operation
  input-tangles
  output-tangle
  step-kernel
  qubit
  qubit-index
  size
  )

(defstruct (cnc-entanglement-operation (:include cnc-operation))
  qubit-2
  qubit-2-index
  )

(defstruct (cnc-kronecker-operation (:include cnc-operation))
  size-2
  input-tangle-2)

(defstruct (cnc-measurement-operation (:include cnc-operation))
  angle
  s-signal
  t-signal
  )

(defstruct (cnc-correction-operation (:include cnc-operation))
  signal
  )

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

;; (defstruct cnc-prescription
;;   (tag  nil :type tag-collection)
;;   (step nil :type cnc-step))

;; (defgeneric listify-prescriptions (obj))
;; (defmethod listify-prescriptions ((obj sequence))
;;   (mapcar #'listify-prescriptions obj)
;; )
;; (defmethod listify-prescriptions ((obj cnc-prescription))
;;   (list (cons (cnc-prescription-tag obj)
;; 	      (cnc-prescription-step obj))))

;; ;;;; CNC-NODE-PRESCRIPTIONS
;; (defgeneric cnc-node-prescriptions (content node))
;; (defmethod cnc-node-prescriptions (obj node))
;; (defmethod cnc-node-prescriptions ((obj cnc-input-tangle) node)
;;   (list (make-cnc-prescription
;; 	 :tag  (cnc-input-tangle-generator-tag-collection obj)
;; 	 :step (cnc-input-tangle-step-kernel obj))))
;; (defmethod cnc-node-prescriptions ((obj cnc-output-tangle) node)
;;   (list (make-cnc-prescription
;; 	 :tag (cnc-output-tangle-tag-collection obj)
;; 	 :step (cnc-output-tangle-step-kernel obj))))
;; (defmethod cnc-node-prescriptions ((obj cnc-operation) node)
;;   (list (make-cnc-prescription 
;; 	 :tag  (cnc-operation-input-tag-collection obj)
;; 	 :step (cnc-operation-step-kernel obj))))
;; (defmethod cnc-node-prescriptions ((obj cnc-measurement-operation) node)
;;   (list (make-cnc-prescription 
;; 	 :tag  (cnc-measurement-operation-input-tag-collection obj)
;; 	 :step (cnc-measurement-operation-step-kernel obj))))

(defun make-tangle-name-pair () 
  ;; I want both names to use the same number
  (prog1 (cons (concatenate 'string "tangle_" (format nil "~D" *gensym-counter*))
	       (concatenate 'string "do_on_tangle_" (format nil "~D" *gensym-counter*)))
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

(defmethod change-node-content ((tangle mcg::tangle))
  (let ((tangle-name-pair (make-tangle-name-pair)))
    (make-cnc-tangle :tangle tangle
		     :size (expt 2 (length (mcg::tangle-qubits
  tangle)))
		     :name (string (gensym "tangle_")))))


(let ((the-cnc-signal (make-cnc-signal :item-collection (cnc::make-cnc-item-collection :name "signals"
										       :type "bool"))))
  (defmethod change-node-content ((signals mcg::signal-map))
    the-cnc-signal))

(defmethod change-node-content ((operation mcg::ag-entanglement))
  (make-cnc-entanglement-operation :qubit (mcg::ag-entanglement-qubit-1 operation)
				   :step-kernel (get 'E 'kernel)
				   :qubit-2 (mcg::ag-entanglement-qubit-2 operation)))

(defmethod change-node-content ((operation mcg::kronecker-operation))
  (make-cnc-kronecker-operation :step-kernel (get 'kron 'kernel)))

(defmethod change-node-content ((operation mcg::ag-measurement))
  (make-cnc-measurement-operation :qubit (mcg::ag-measurement-qubit operation)
				  :angle (mcg::ag-measurement-angle operation)
				  :s-signal (mcg::ag-measurement-s-signal operation)
				  :t-signal (mcg::ag-measurement-t-signal operation)
				  :step-kernel (get 'M 'kernel)
				  :do-m-tag-collection 
				  (cnc::make-cnc-tag-collection :name (symbol-to-string (gensym "do_m_")))))

(defmethod change-node-content ((operation mcg::ag-X-correction))
  (make-cnc-correction-operation :qubit (mcg::ag-correction-qubit operation)
				 :signal (mcg::ag-correction-s-signal operation)
				 :step-kernel (get 'X 'kernel)))

(defmethod change-node-content ((operation mcg::ag-Z-correction))
  (make-cnc-correction-operation :qubit (mcg::ag-correction-qubit operation)
				 :signal (mcg::ag-correction-s-signal operation)
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
      (setf qubit-index (when qubit (mcg::qubit-tensor-index qubit (cnc-tangle-tangle input-cnc-tangle)))
            input-item-collection (cnc-tangle-item-collection input-cnc-tangle)
	    input-tag-collection (cnc-tangle-tag-collection input-cnc-tangle)
	    output-item-collection (cnc-tangle-item-collection output-cnc-tangle)
	    output-tag-collection (cnc-tangle-tag-collection output-cnc-tangle)
	    signal-item-collection (when signals (cnc-signal-item-collection signals))
	    size (cnc-tangle-size input-cnc-tangle)))))

(defmethod instantiate-node-content (content node cnc-graph))

(defmethod instantiate-node-content ((operation cnc-entanglement-operation) node cnc-graph)
  (with-slots (qubit-2 qubit-2-index)
      operation
    (setf qubit-2-index 
	  (mcg::qubit-tensor-index qubit-2 
				   (cnc-tangle-tangle (get-cnc-tangle (node-upstream-nodes node)))))))

(defmethod instantiate-node-content ((operation cnc-kronecker-operation) node cnc-graph)
  (with-slots (step-kernel input-item-collection input-tag-collection
	       input2-item-collection input2-tag-collection 
	       output-item-collection output-tag-collection 
	       size size-2
	       )
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
)))


(defmethod instantiate-node-content ((cnc-tangle cnc-tangle) node cnc-graph)
  ;; if tangle is an input or output node, add a cnc-step to it
  (cond ((input-node-p node cnc-graph)
	 (setf (node-content node)
	       (make-input-cnc-tangle cnc-tangle)))
	((output-node-p node cnc-graph)
	 (setf (node-content node)
	       (make-output-cnc-tangle cnc-tangle)))))

;;;; MAKE-IO-CNC-TANGLE

(defun make-input-cnc-tangle (cnc-tangle)
  (with-slots (tangle
	       size
	       item-collection
	       tag-collection)
      cnc-tangle
    (make-cnc-input-tangle :tangle tangle
			   :size size
			   :item-collection item-collection
			   :tag-collection tag-collection)))

(defun make-output-cnc-tangle (cnc-tangle)
  (with-slots (tangle
	       size
	       item-collection
	       tag-collection)
      cnc-tangle
    (make-cnc-output-tangle :tangle tangle
			    :size size
			    :item-collection item-collection
			    :tag-collection tag-collection)))

;;;; CNC PROGRAM GENERATION

;;; OPERATION-TO-STEP
(defgeneric add-operation-step (operation (program cnc::cnc-program)))

(defmethod add-operation-step ((operation cnc-operation) (program cnc::cnc-program))
  (with-slots (input-item-collection
	       output-item-collection
	       input-tag-collection
	       output-tag-collection
;	       signal-item-collection
	       step-kernel
	       qubit-index
	       size) operation
    (add-tangle-item)
 (cnc::make-cnc-step-collection :name (cnc::kernel-name (cnc-operation-step-kernel op))
				:kernel (cnc-operation-step-kernel op)
				:produces (list output-item-collection)
				:consumes (list input-item-collection)
				:controls (list output-tag-collection)
				:parameter-bindings (list size qubit-index))))

(defmethod operation-to-step (()))

(defun cnc-graph-to-program (cnc-graph)
  (loop for node in (mcg::graph-nodes cnc-graph)
	for node-content = (mcg::node-content node)
	with program = (cnc::make-cnc-program 
			:utility-function-bodies (list *source-tensor-permute-function*
						       *source-permute-function*
						       *source-compact-index-function*)
			:source-kernel (get 'source 'kernel)
			:sink-kernel (get 'sink 'kernel))
	when (cnc-operation-p node-content)
	  collect (operation-to-step node-content)
	  )
  )

#+nil(defun construct-cnc-program-from-graph (cnc-graph)
  (loop
    for node in (graph-nodes cnc-graph)
    for node-content = (node-content node)
    when (cnc-operation-p node-content)
      collect 
        into steps
      and collect (cons (symbol-name (cnc-step-name (cnc-operation-step-kernel node-content)))
			(item-collection-name (cnc-operation-output-item-collection node-content)))
        into produces
    ;; adding tuners
    when (and (cnc-operation-p node-content)
	      (cnc-operation-tuner-name node-content))
      collect (cons (symbol-name (cnc-step-name (cnc-operation-step-kernel node-content)))
		    (cons  (cnc-operation-tuner-name node-content)
			   (cnc-operation-tuner-args node-content)))
	into tuned-steps
    finally
       (let* ((steps       (mapcar #'cnc-prescription-step prescriptions))
	      (tag-names   (mapcar (compose #'tag-collection-name #'cnc-prescription-tag) prescriptions))
	      (step-names  (mapcar (compose #'symbol-name #'cnc-step-name) steps))
	      (step-bodies (mapcar #'cnc-step-body steps)))
	 (return (make-cnc-program :items         (mapcar #'item-collection-name items)
				   :item-sizes    item-sizes
				   :tags          (append dangling-tag-names tag-names)
				   :step-names    step-names
				   :step-bodies   step-bodies
				   :input-tags    (mapcar #'tag-collection-name generator-tag-collections)
				   :prescriptions (pairlis tag-names step-names)
				   :consumes      consumes
				   :produces      produces
				   :tuned-steps   tuned-steps)))))


(defun compile-to-cnc ()
  (sb-sys:enable-interrupt sb-unix:sigint #'(lambda () (sb-ext:quit)))
  (format t " mcc> ")
  (finish-output)
  (let ((mc-program (read *standard-input* nil)))
    (format t "Generating MC program graph... ")
    (let ((mc-graph (compile-mc mc-program)))
;      (show-dot mc-graph)
      (format t "done~%Generating CnC-specific graph... ")
      (let ((g (mc-graph-to-cnc-graph mc-graph)))
	(show-dot g)
	(format t "done~%Collecting data for CnC code generation... ")
	g
	#+nil(let ((cnc-program (construct-cnc-program-from-graph g)))
	  (with-slots (items item-sizes tags step-names step-bodies
		       input-tags prescriptions tuned-steps produces consumes)
	      cnc-program
	    (format t "done~%Beginning code generation.~%")
	    #+nil(cnc-gen:build items tags step-names step-bodies
			   input-tags prescriptions item-sizes consumes produces)
	    'ok))))))


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
      ;;;; cnc nodes
      (cnc-tangle (format nil "Tangle [ ~{~S ~}]" 
			  (mcg::tangle-qubits (cnc-tangle-tangle content))))
      (cnc-entanglement-operation 
       (format nil "~A (~A, ~A)"
	       (cnc::kernel-name (cnc-operation-step-kernel content)) 
	       (cnc-entanglement-operation-qubit content)
	       (cnc-entanglement-operation-qubit-2 content)))
      (cnc-operation
       (format nil "~A (~A)"
	       (cnc::kernel-name (cnc-operation-step-kernel content)) 
	       (cnc-operation-qubit content)))
      (cnc-signal 
       "Signal-map")
      (otherwise (node-label node)))))

(defun dot-node-shape (node graph)
  (typecase (node-content node)
    ((or mcg::ag-operation mcg::operation cnc-operation) "ellipse")
    ((or mcg::tangle cnc-tangle) "rectangle")
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


(defvar *source-tensor-permute-function*
"
static unsigned int tensor_permute(const unsigned int i,const unsigned int siz, 
                                   const unsigned int i1, const unsigned int i2) {
  // MAKE SURE i1 >= i2 !!!
  // const int o = i1;
  const unsigned int m = 2;
  const unsigned int n = (i1 / i2);
  const unsigned int p = i2;
   return 
    i 
    + p * ( n - 1 ) * floor( i / p      )
    - p * (m*n - 1) * floor( i / (m*p)  )
    + p * n * (n-1) * floor( i / (m*p*n));
}")
  
 (defvar *source-permute-function*
"
static unsigned int permute(const unsigned int i,const unsigned int m, 
                            const unsigned int n) {
  return n * ( i % m ) + floor( i / m ); 
}")
  
 (defvar *source-compact-index-function*
"
static unsigned int compact_bit_index(const unsigned int i, const unsigned int bit) {
  return ((i - i % (2 * bit)) >> 1) + i % bit;
}
")