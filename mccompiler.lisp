
(defpackage :mc-compiler
  (:nicknames :mcc)
  (:use :cl :mcg :cnc :cnc-gen)
    (:import-from alexandria if-let compose)
  (:export compile-to-cnc))

(in-package :mcc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (debug 3)))
  ;; these get used at macro-expand time
  (defun symbol-to-string (sym)
    (string-downcase (symbol-name sym)))
  (defun symbol-list-to-string (symlist)
    (mapcar #'symbol-to-string symlist))
  )


(defvar *kron-depends-body*
  "
for( int t(0) ; t < size_2 ; ++t ) {
  dC.depends( tangle_2 , t );
}
")

(defkernel E ((:consumes in_items) 
	      (:produces out_items) 
	      (:controls out_tags)
	      (:parameters (size int) 
			   (qid_1 int) 
			   (qid_2 int)))
"
amplitude a_i;
//const int q1 = qid_1 > qid_2 ? qid_1 : qid_2;
//const int q2 = qid_1 > qid_2 ? qid_2 : qid_1;

in_items.get( t , a_i );

//const int f_i = tensor_permute( t , size , q1, q2 );
//if (f_i % 4 == 3)

if( t & qid_1 && t & qid_2 ) 
  a_i = - a_i;

out_items.put( t , a_i );
out_tags.put( t );
")

(defkernel kron ((:consumes tangle_1 tangle_2)
		 (:produces tangle_out)
		 (:controls tag_out)
		 (:parameters (size_2 int))
		 (:depends (tangle_2 size_2)
			   *kron-depends-body*))
  "
amplitude amp_1;
amplitude amps[size_2];

tangle_1.get(t,amp_1);
for(int i(0);i<size_2;++i) {
  tangle_2.get(i,amps[i]);
}
for(int i(0);i<size_2;++i) {
  const int new_index( t * size_2 + i );
  tangle_out.put( new_index, amp_1 * amps[i] );
  tag_out.put(new_index);
}
")

(defvar *M-depends-body*
   "
if ( (t & qid) == 0 ) {
  //dC.depends( in_tangle , t );
  dC.depends( in_tangle , t + qid);
}")

(defkernel M ((:consumes in_tangle) 
	      (:produces out_tangle) 
	      (:controls out_tags)
	      (:parameters (size int) 
			   (qid int)
			   (angle double))
	      (:depends (in_tangle qid)
			*M-depends-body*))
  "
const int i = t;

if ( (i & qid) == 0 ) {
  amplitude a_i1;
  amplitude a_i2;
  const amplitude phi_1 = std::exp(amplitude(0,-angle));
  const int i2 = i + qid;
  in_tangle.get( i  , a_i1 );
  in_tangle.get( i2 , a_i2 );
  const amplitude new_amp = a_i1 - a_i2 * phi_1;
  const int new_index = compact_bit_index(i,qid);
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
/*
const int m = qid;
const int n = size / qid;
bool signal=true;
*/

in_tangle.get( i , a_i );
/* 
const int target_index = 
  signal ? permute( permute(i, n, m)^1, m, n )
         : i; 
*/
const int target_index = i ^ qid;

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

(defkernel emx_r ((:consumes in_tangle) 
		  (:produces out_tangle) 
		  (:controls out_tags)
		  (:parameters (e_qid_1 int)
			       (e_qid_2 int)
			       (m_qid   int)
			       (angle   double)
			       (x_qid   int)))
  "
/*        even  odd  */
const amplitude phi = std::exp(amplitude(0,-angle));

if( m_qid != 1 ) {  // fresh qubit is NOT the measured qubit
  // only do something half of the time (e.g. measure bit set)
  if( t & (m_qid>>1) ) {  // t is tag of 'odd' amp? (measured bit is set)
    /*  fetching readied amplitude and it's partner needed in
	measurement below */
    amplitude amp_odd, amp_even;
    const int amp_odd_t   = t;
    const int amp_even_t  = t^(m_qid>>1);  // m_qid's tag _before_ tensor
    in_tangle.get(amp_odd_t,  amp_odd);
    in_tangle.get(amp_even_t, amp_even);
    
    /*      m_qid--v
       amp_even|...0...> + amp_odd|...1...>   
    */ 
    /*  tensor product with fresh qubit 0.5|0> + 0.5|1> */
     amplitude amps[4] = 
	{ amp_even, amp_even,   
	  amp_odd,  amp_odd };

    const int tags[4] = { amp_even_t<<1, (amp_even_t<<1) + 1,
			    amp_odd_t<<1,  (amp_odd_t<<1)  + 1 };

    for( int i(0); i<4; ++i ) {  // hoping this gets SSE'ed
      amps[i] *= 0.5;
    }
    /*
	amps[0]|...0...>|0> + amps[1]|...0...>|1> +  
	amps[0]|...1...>|0> + amps[2]|...1...>|1>  
     */

    /*  controlled-Z  */
    for( int i(0); i<4; ++i ) {  // hoping this gets SSE'ed
      if( tags[i] & e_qid_1 && tags[i] & e_qid_2 ) 	
	amps[i] *= -1;
    }
    
    /*  measurement  */
    const amplitude new_amp_0 = (amps[0] - amps[2] * phi);
    const amplitude new_amp_1 = (amps[1] - amps[3] * phi);
    int new_tag_0 = compact_bit_index(t<<1,m_qid);
    int new_tag_1 = compact_bit_index(t<<1,m_qid) + 1;
    assert(new_tag_0 == (new_tag_1 ^ 1));
    /*  X-operation  */
    new_tag_0 ^= x_qid;
    new_tag_1 ^= x_qid;

    /*  output       */
    out_tangle.put(new_tag_0, new_amp_0);
    out_tangle.put(new_tag_1, new_amp_1);
    out_tags.put(new_tag_0);
    out_tags.put(new_tag_1);
  }
 }
 else { // fresh qubit IS the measured qubit
        // this should not happen a lot
   amplitude amp1, amp2;
   in_tangle.get(t,amp1);
   /*  tensor        */
   const int t1 = t<<1;
   const int t2 = (t<<1)+1;
   /*  controlled-Z  */
   amp1 = (t1 & e_qid_1 && t1 & e_qid_2) ?
     -amp1 * 0.5 : amp1 * 0.5;
   amp2 = (t2 & e_qid_1 && t2 & e_qid_2) ?
     -amp1 * 0.5 : amp1 * 0.5;
   /*  measurement   */
   const amplitude new_amp = amp1 - amp2 * phi;
   /*  X-operation   */
   const int new_tag = t ^ x_qid;
   out_tangle.put(new_tag, new_amp); 
   out_tags.put(new_tag);
 }
")



(defkernel source ((:produces out_items)
		   (:controls out_tags)
		   (:parameters (size int)))
"
for( int i(0) ; i < size ; ++i ) {
  out_items.put(i, 1/sqrt(size));
  out_tags.put(i);
}
")

(defkernel sink ((:consumes in_items)
		 (:parameters (size int)))
"
static int n(0);
printf(\"tangle %d:\\n\", n++);
for( int i(0) ; i < size ; ++i ) {
  amplitude amp;
  in_items.get(i, amp);
  printf(\"  [%d]: (%1.4f,%1.4f) \\n\", i, amp.real(), amp.imag());
}
")


(defvar *source-tensor-permute-function*
"
static int tensor_permute(const int i,const int siz, 
                                   const int i1, const int i2) {
  // MAKE SURE i1 >= i2 !!!
  // const int o = i1;
  const int m = 2;
  const int n = (i1 / i2);
  const int p = i2;
   return 
    i 
    + p * ( n - 1 ) * floor( i / p      )
    - p * (m*n - 1) * floor( i / (m*p)  )
    + p * n * (n-1) * floor( i / (m*p*n));
}")
  
 (defvar *source-permute-function*
"
static int permute(const int i,const int m, 
                   const int n) {
  return n * ( i % m ) + floor( i / m ); 
}")
  
 (defvar *source-compact-index-function*
"
static int compact_bit_index(const int i, const int bit) {
  return ((i - i % (2 * bit)) >> 1) + i % bit;
}
")

#|

// E e_qid1 e_qid2, M m_qid, X x_qid
struct operation_j2r: public CnC::step_tuner<> {
  int execute( const int& t, constext& c ) const {
    
  const int t1 = t << 1;
  const int t2 = (t << 1) ^ 1;

  amplitude amp1, amp2;
    
  if( t1 & m_qid )
    return CnC::CNC_Success;

  in_tangle.get(t,amp1);
  if( m_qid == 1 ) {
    amp2 = 0.5;
  }
  else {
    in_tangle.get(t ^ (m_qid >> 1),amp2);

  amplitude a1 = amp1 * 0.5;
  amplitude a2 = amp1 * 0.5;
  amp1itude a3 = amp2 * 0.5;
  amp1itude a4 = amp2 * 0.5;

  const int e_q1 = e_qid1 > e_qid2 ? e_qid1 : e_qid2;
  const int e_q2 = e_qid1 > e_qid2 ? e_qid2 : e_qid1;
  
  const int f_i  = tensor_permute( t1 , size , q1, q2 );
  const int f_i2 = tensor_permute( t2 , size , q1, q2 );
  
  if (f_i % 4 == 3)
    a1 = - a1;
  if (f_i2 % 4 == 3)
    a2 = - a2;
  
  return CnC::CNC_Success;
  }

};

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 1. MC GRAPH TO CNC PROGRAM  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mc-graph-to-cnc-program (mc-graph)
  (let ((swap-table (make-hash-table)))
    ;; preliminary pass: create elements, only used to fix object identities
    (loop for node in (graph-nodes mc-graph)
	  do (setf (gethash node swap-table)
		   (prototype-mc-node (node-content node))))
    ;; first pass: process each node in-place, fill in info from AG
    (loop for node in (graph-nodes mc-graph)
	  do (process-first-pass (node-content node) 
				 node
				 swap-table))
    ;; second pass: process information from dependencie (controls,
    ;; prescribes)
    (loop for node in (graph-nodes mc-graph)
	  do (process-second-pass (node-content node) 
				  node
				  swap-table))

    ;; third pass: coarsening optimizations
    (loop for node in (graph-nodes mc-graph)
	  when (match-emc-p node mc-graph)
	    do (replace-with-superoperation node swap-table :emc)
;	    do (format t "EMC match in: ~A~%" (node-label node))
	       
	  )

    ;; collect all information and construct cnc-program object
    (loop for node being each hash-key in swap-table 
	    using (hash-value collection)
	  when (cnc::cnc-item-collection-p collection)
	    collect collection into items 
	    and collect (cnc::cnc-item-collection-associated-tags collection)
	      into tags
	    and if (input-node-p node mc-graph)
	      collect collection into inputs
	    else if (output-node-p node mc-graph)
	      collect collection into outputs
	  when (cnc::cnc-step-collection-p collection)
	    collect collection into steps
	  finally 
	     (return (cnc::make-cnc-program 
		      :items items
		      :steps steps
		      :tags tags
		      :utility-function-bodies 
		      (list *source-permute-function*
			    *source-tensor-permute-function*
			    *source-compact-index-function*)
		      :source-kernel (get 'source 'kernel)
		      :sink-kernel (get 'sink 'kernel)
		      :input-item-collections inputs
		      :output-item-collections outputs)))))


(defun producing (node swap-table)
  (remove-if-not #'cnc::cnc-item-collection-p 
		 (mapcar #'(lambda (node) 
			     (gethash node swap-table))
			 (mcg::node-downstream-nodes node))))
(defun consuming (node swap-table)
  (remove-if-not #'cnc::cnc-item-collection-p 
		 (mapcar #'(lambda (node) 
			     (gethash node swap-table))
			 (mcg::node-upstream-nodes node))))
(defun consuming-tangles (node)
  (remove-if-not #'mcg::tangle-p 
		 (mapcar #'mcg::node-content 
			 (mcg::node-upstream-nodes node))))


;;; PROTOTYPE-MC-NODE
(defgeneric prototype-mc-node (content))

(defmethod prototype-mc-node (content)
  "fallthrough, at the moment only ag-signal-map should fall through"
  nil)

(defmethod prototype-mc-node ((content mcg::ag-operation))
  (cnc::make-cnc-step-collection))

(defmethod prototype-mc-node ((content mcg::kronecker-operation))
  (cnc::make-cnc-step-collection))

(defmethod prototype-mc-node ((content mcg::tangle))
  (cnc::make-cnc-item-collection))


;;; PROCESS-FIRST-PASS
(defgeneric process-first-pass (content node swap-table))

(defmethod process-first-pass (content node swap-table)
  "fallthrough, at the moment only ag-signal-map should fall through"
  nil)

(defmethod process-first-pass ((content mcg::tangle) node swap-table)
  (let ((item-collection (gethash node swap-table))
	(id (gensym "id")))
    (with-accessors ((name cnc::cnc-item-collection-name) 
		     (type cnc::cnc-item-collection-type) 
		     (size cnc::cnc-item-collection-size)
		     ;(tuner cnc::cnc-item-collection-tuner)
		     (associated-tags cnc::cnc-item-collection-associated-tags))
	item-collection
      (setf name (format nil "tangle_~A" id)
	    type "amplitude"
	    size (mcg::tangle-size content))
      (setf associated-tags 
	    (cnc::make-cnc-tag-collection :name (format nil "tag_tangle_~A" id)))

      )))

(defmethod process-first-pass ((content mcg::operation) 
			       node
			       swap-table)
  "ag-operation invariant: consumes one, produces one"
  (let ((step-collection (gethash node swap-table)))
    (with-accessors ((name cnc::cnc-step-collection-name)
		     (kernel cnc::cnc-step-collection-kernel) 
		     (produces cnc::cnc-step-collection-produces) 
		     (consumes cnc::cnc-step-collection-consumes))
	step-collection
      (setf name     (gensym (format nil "~A_" (process-name content)))
	    kernel   (get (process-name content) 'kernel)
	    produces (producing node swap-table)
	    consumes (consuming node swap-table)))))

;;; PROCESS-SECOND-PASS

(defmethod process-second-pass (content node swap-table)
  nil)

(defmethod process-second-pass ((content mcg::operation)
				node
				swap-table)
  (let* ((step-collection (gethash node swap-table)))
    (with-accessors ((produces cnc::cnc-step-collection-produces)
		     (consumes cnc::cnc-step-collection-consumes)
		     (controls cnc::cnc-step-collection-controls)
		     (parameter-bindings cnc::cnc-step-collection-parameter-bindings))
	step-collection
      (setf controls (mapcar #'cnc::cnc-item-collection-associated-tags
			     produces)
	    parameter-bindings (process-bindings content
						 (consuming-tangles node))))))

(defun downstream-steps (node swap-table)
  (declare (type mcg::node node)
	   (type hash-table swap-table))
  (flet ((swap (node) (gethash node swap-table)))
    (remove-if-not #'cnc-step-collection-p 
		   (mapcar #'swap (node-downstream-nodes node)))))

(defmethod process-second-pass ((content mcg::tangle) node swap-table)
  (when (node-downstream-nodes node)
    (let* ((items (gethash node swap-table))
	   (tags (cnc-item-collection-associated-tags items))
	   (consuming-node (first (node-downstream-nodes node)))
	   (steps (first (downstream-steps node swap-table)))
	   (consumes (cnc-step-collection-consumes steps)))
      (assert (= 1 (length (node-downstream-nodes node))))
      (flet ((aux-consumed-p () 
	       (and (= (length consumes) 2)
		    (equal tags 
			   (cnc-item-collection-associated-tags (second
								 consumes))))))
	(unless (aux-consumed-p)
	  (setf (cnc-tag-collection-prescribes tags) (list steps)))
	(let ((get-count (consumed-count (node-content consuming-node)
					 (gethash consuming-node swap-table)
					 node)))
	  (setf (cnc::cnc-item-collection-tuner items)
		(cnc::make-cnc-item-tuner :get-count get-count)))))))

(defgeneric consumed-count (operation step tangle-node))
(defmethod consumed-count ((operation mcg::ag-operation) step tangle-node)
  1)
(defmethod consumed-count ((operation mcg::ag-measurement) step tangle-node)
  2)
(defmethod consumed-count ((operation mcg::kronecker-operation) 
		      step 
		      tangle-node)
  (let* ((consumes (cnc-step-collection-consumes step))
	 (tangle (node-content tangle-node))
	 (tangle-1 (first consumes))
	 (tangle-2 (second consumes)))
    (assert (= (length consumes) 2))
    (if (equal tangle tangle-1)
	(cnc-item-collection-size tangle-2)
	(cnc-item-collection-size tangle-1))))



#+nil(defmethod process-mc-node ((content mcg::kronecker-operation) 
			    node
			    swap-table)
  "slight variation on ag-operation: consumes two, produces one"
  (assert (= (length (producing node swap-table)) 2))
  (let ((step-collection (gethash node swap-table)))
))

(defun process-name (content)
  (typecase content
    (mcg::ag-x-correction 'X)
    (mcg::ag-z-correction 'Z)
    (mcg::ag-entanglement 'E)
    (mcg::ag-measurement 'M)
    (mcg::kronecker-operation 'kron)))


(defgeneric process-bindings (content consuming-tangles))

(defmethod process-bindings ((content mcg::ag-correction)
			     consuming-tangles)
  (list (cnc::make-actual-parameter 
	 :name 'size
	 :value (mcg::tangle-size (first consuming-tangles)))
	(cnc::make-actual-parameter 
	 :name 'qid
	 :value (mcg::qubit-tensor-index (mcg::ag-correction-qubit content) 
					 (first consuming-tangles)))))

(defmethod process-bindings ((content mcg::ag-entanglement)
			     consuming-tangles)
   #+nil(:parameters (size int) 
		      (qid_1 int) 
		      (qid_2 int))
  (list (cnc::make-actual-parameter 
	 :name 'size
	 :value (mcg::tangle-size (first consuming-tangles)))
	(cnc::make-actual-parameter 
	 :name 'qid_1
	 :value (mcg::qubit-tensor-index (mcg::ag-entanglement-qubit-1 content) 
					 (first consuming-tangles)))
	(cnc::make-actual-parameter 
	 :name 'qid_2
	 :value (mcg::qubit-tensor-index (mcg::ag-entanglement-qubit-2 content) 
					 (first consuming-tangles)))))

(defmethod process-bindings ((content mcg::ag-measurement)
			     consuming-tangles)
  #+nil(:parameters (size int) 
		    (qid int)
		    (angle double))
  (list (cnc::make-actual-parameter 
	 :name 'size
	 :value (mcg::tangle-size (first consuming-tangles)))
	(cnc::make-actual-parameter 
	 :name 'qid
	 :value (mcg::qubit-tensor-index 
		 (mcg::ag-measurement-qubit content)
		 (first consuming-tangles)))
	(cnc::make-actual-parameter 
	 :name 'angle
	 :value (mcg::ag-measurement-angle content))))

(defmethod process-bindings ((content mcg::kronecker-operation)
			     consuming-tangles)
  (list (cnc::make-actual-parameter 
	 :name 'size_2
	 :value (mcg::tangle-size (second consuming-tangles)))))

(defun select-downstream-nodes (predicate node)
  (remove-if-not predicate (node-downstream-nodes node)
		 :key #'node-content))

(defun walk-next-operation-node (node)
  (let ((tangle-nodes (select-downstream-nodes #'mcg::tangle-p 
					       node)))
    (and tangle-nodes
	 (= (length tangle-nodes) 1)
	 (select-downstream-nodes #'mcg::operation-p 
				  (first tangle-nodes))
	 (first (select-downstream-nodes #'mcg::operation-p 
					 (first tangle-nodes))))))
    
(defun match-emc-p (node mc-graph)
  "Checks several conditions: 
  consecutive operations: tensor, E, M and X; 
  second (right side) tangle only contains a fresh qubit"
  (let ((content (node-content node)))
    (and (mcg::kronecker-operation-p content)
	 (let* ((tangle-nodes (node-upstream-nodes node)))
	   ;; checking if the tangle on the right only has a fresh qubit
	   (and (= 2 (mcg::tangle-size (node-content (second tangle-nodes))))
		(input-node-p (second tangle-nodes) mc-graph)))
	 (let ((E-node (walk-next-operation-node node)))
	   (and (mcg::ag-entanglement-p (node-content E-node))
		(let ((M-node (walk-next-operation-node E-node)))
		  (and (mcg::ag-measurement-p (node-content M-node))
		       ;;account for signal-node
		       (walk-next-operation-node M-node)
		       (mcg::ag-x-correction-p (node-content 
						(walk-next-operation-node
						 M-node))))))))))

(defun select-and-set-param (step old-param-name new-param-name)
  (let ((param (find old-param-name (cnc-step-collection-parameter-bindings step)
		     :key #'actual-parameter-name)))
    (setf (cnc::actual-parameter-name param) new-param-name)
    param))


(defmethod replace-with-superoperation ((node mcg::node) 
					(swap-table hash-table) 
					(type (eql :emc)))
  "Creates new cnc operation and puts it in place of the old EMC
operations, manipulating cnc objects in swap table.  
Hacky implementation, ideally you would want to take a (sub-)graph
here and produce a new graph with correct swap-table."
  (flet ((swap (node) (gethash node swap-table))
	 (forget-node (node) (assert (remhash node swap-table))))
    (let* ((kron-node          node)
	   (fresh-tangle-node  (second (node-upstream-nodes node)))
	   (tangle-kron-E-node (first (node-downstream-nodes node)))
	   (E-node             (first (node-downstream-nodes tangle-kron-E-node)))
	   (tangle-E-M-node    (first (node-downstream-nodes E-node)))
	   (M-node             (first (node-downstream-nodes tangle-E-M-node)))
	   (tangle-M-X-node    (first (node-downstream-nodes M-node)))
	   (X-node             (first (node-downstream-nodes tangle-M-X-node)))
	   (emx-operation 
	     (cnc::make-cnc-step-collection 
	      :name     (gensym "emx_")
	      :kernel   (get 'emx_r 'kernel)
	      :produces (cnc-step-collection-produces (swap X-node))
	      :consumes (list (first (cnc-step-collection-consumes (swap kron-node))))
	      :controls (cnc-step-collection-controls (swap X-node))
	      :parameter-bindings 
	      (mapcar #'select-and-set-param
		      (mapcar #'swap 
			      (list E-node E-node M-node M-node X-node))
		      '(  qid_1   qid_2   qid angle   qid)
		      '(e_qid_1 e_qid_2 m_qid angle x_qid)))))

      (let* ((consumed-item (first (cnc-step-collection-consumes emx-operation)))
	     (prescribing-tags (cnc-item-collection-associated-tags consumed-item)))
	(print "consuming: ")
	;; change prescription
	(setf (cnc-tag-collection-prescribes prescribing-tags) 
	      (list emx-operation))
	;; currently a hack, letting the kron-node stand in for the
	;; emx operation, better solution is to change the mc-graph
	(setf (gethash kron-node swap-table) emx-operation)
	
	;; remove all above nodes from swap-table, except kron-node
	;; that will act as achor for the emc-operation
	(mapcar #'forget-node (list E-node M-node X-node))
	(mapcar #'forget-node
		(list fresh-tangle-node tangle-kron-e-node
		      tangle-e-m-node tangle-m-x-node)))

	#+nil((:consumes in_tangle) 
	      (:produces out_tangle) 
	      (:controls out_tags)
	      (:parameters (e_qid_1 int)
			   (e_qid_2 int)
			   (m_qid   int)
			   (angle   double)
			   (x_qid   int)))
      )))

(defun compile-to-cnc (&optional program)
  (sb-sys:enable-interrupt sb-unix:sigint #'(lambda () (sb-ext:quit)))
  (format t " mcc> ")
  (finish-output)
  (let ((mc-program (if program 
			program 
			(read *standard-input* nil))))
    (format t "Generating MC program graph... ")
    (let ((mc-graph (compile-mc mc-program)))
      (format t "done~%Collecting data for CnC code generation... ")
;      (mcg::show-dot mc-graph)
      (let ((cnc-program (mc-graph-to-cnc-program mc-graph)))
	(cnc::show-dot cnc-program)
;	(inspect cnc-program)
	(format t "done~%Beginning code generation.~%")
	(build cnc-program)
	'ok))))

#+nil(let ((mc-program '((E 1 2) (E 3 4) (E 2 4) (M 1) (X 2 (q 1)))))
  (compile-to-cnc mc-program))

#+nil(let ((mc-program '((X 1) (E 3 4) (E 2 3) (E 1 3) (M 2) (M 3) (Z 1 (q
								    2))
		    (Z 4 (q 2)) (X 4 (q 3)) (X 1))))
  (compile-to-cnc mc-program))

#+nil(let ((mc-program '((E 1 0) (M 1 (- 0)) (X 0 (Q 1)) (E 0 5) (M 0 (- -0.7853981633974483))
 (X 5 (Q 0)) (E 11 5) (E 5 12) (M 5 (- 0)) (X 12 (Q 5)) (E 11 12) (E 12 19)
 (M 12 (- -0.7853981633974483)) (X 19 (Q 12)) (E 11 19) (E 19 26) (M 19 (- 0))
 (X 26 (Q 19)) (E 26 31) (M 26 (- 1.5707963267948966)) (X 31 (Q 26)) (E 31 36)
 (M 31 (- 0)) (X 36 (Q 31)) (E 11 41) (M 11 (- 0.7853981633974483))
 (X 41 (Q 11)) (E 41 46) (M 41 (- 0)) (X 46 (Q 41)) (E 36 51) (M 36 (- 0))
 (X 51 (Q 36)))))
  (compile-to-cnc mc-program))