
(defpackage :cnc-model
  (:nicknames :cnc)
  (:use :cl)
  (:import-from alexandria with-gensyms ensure-symbol compose)
  (:export defkernel distinct-kernels step-kernel-names step-names
	   item-names item-types tag-names match-parameter-bindings
	   input-item-collections input-tag-collections
	   output-item-collections output-tag-collections cnc-program
	   cnc-program-prescriptions cnc-program-consumes 
	   cnc-program-produces cnc-program-controls
	   match-parameter-values kernel-p kernel kernel-name
	   kernel-consumes kernel-parameters kernel-produces
	   kernel-body kernel-tuners kernel-controls formal-parameter
	   formal-parameter-name formal-parameter-type
	   make-formal-parameter cnc-step-collection-produces
	   cnc-step-collection-consumes cnc-step-collection-controls
	   cnc-step-collection-kernel cnc-program-items cnc-step-collection-name
	   cnc-program-tags cnc-program-steps
	   cnc-step-collection-parameter-bindings
	   actual-parameter-value actual-parameter-name
	   cnc-item-collection cnc-tag-collection cnc-step-collection
	   cnc-item-collection-name cnc-tag-collection-name 
	   cnc-tag-collection-prescribes cnc-item-collection-size
	   cnc-item-collection-associated-tags
	   cnc-step-collection-p
  ))

(in-package :cnc-model)

(defstruct cnc-program
  items
  steps
  tags
  utility-function-bodies
  source-kernel
  sink-kernel
  input-item-collections
  output-item-collections
  cache
)

(defmacro defun-cached-in-obj (fn-name (obj) &body body)
  (with-gensyms (entry result)
    `(defun ,fn-name (,obj)
       (let ((,entry (assoc ',fn-name (cnc-program-cache ,obj))))
	 (if ,entry
	     (cdr ,entry)
	     (let ((,result (progn ,@body)))
	       (setf (cnc-program-cache ,obj)
		     (cons (cons ',fn-name ,result)
			   (cnc-program-cache ,obj)))
	       ,result))))))

#|(let ((p (make-cnc-program))
      (n 42))
  (defun-cached-in-obj do-some-stuff (obj)
    (format t "prev n was ~d~%" n)
    (incf n))
  (do-some-stuff p)
  (do-some-stuff p)
  (inspect p))|#

(defun-cached-in-obj item-names (program)
  (mapcar #'cnc-item-collection-name (cnc-program-items program)))

(defun-cached-in-obj item-types (program)
  (mapcar #'cnc-item-collection-type (cnc-program-items program)))

(defun-cached-in-obj step-names (program)
  (mapcar #'cnc-step-collection-name (cnc-program-steps program)))

(defun-cached-in-obj step-kernel-names (program)
  (mapcar #'kernel-name
	  (mapcar #'cnc-step-collection-kernel 
		  (cnc-program-steps program)))
  #+nil(mapcar (compose #'kernel-name #'cnc-step-collection-kernel) 
	  (cnc-program-steps program)))

(defun-cached-in-obj tag-names (program)
  (mapcar #'cnc-tag-collection-name (cnc-program-tags program)))

(defun-cached-in-obj distinct-kernels (program)
  (loop for step in (cnc-program-steps program)
	for kernel = (cnc-step-collection-kernel step)
	with kernels
	do (pushnew kernel kernels :test #'equal)
	finally (return kernels)))

(defun-cached-in-obj cnc-program-item-tuners (program)
  (loop for item in (cnc-program-items program)
	with item-tuners
	do (pushnew (cnc-item-collection-tuner item)
		    item-tuners 
		    :test #'equal)
	finally (return item-tuners)))

;;; ITEM COLLECTIONS
(defstruct cnc-item-collection
  name
  type
  size
  tuner
  associated-tags)

(defstruct cnc-item-tuner
  name
  get-count
  (deriving-from "CnC::hashmap_tuner"))

;;; TAG COLLECTIONS
(defstruct cnc-tag-collection
  name
  prescribes
  tuner)
;(defstruct (cnc-input-tag-collection (:include cnc-tag-collection)))
;(defstruct (cnc-output-tag-collection (:include cnc-tag-collection)))
(defstruct cnc-tag-tuner
  (name "CnC::tag_tuner< tbb::blocked_range< int > >")
)


;;; STEP COLLECTIONS
(defstruct cnc-step-collection
  name 
  kernel
  produces
  consumes
  controls
  parameter-bindings
)

(defstruct cnc-step-tuner
  (name (symbol-name (gensym "step_tuner_")))
  depends-body ;depends() body
  parameters
  (deriving-from "CnC::step_tuner<>"))

;;; UTILITIES
(defstruct formal-parameter
  name
  type)
(defstruct actual-parameter
  name
  value)

(defstruct kernel
  name
  produces
  consumes
  controls
  parameters
  body
  tuner
)

(defmacro defkernel (name clauses kernel-body)
  "Creates a kernel instance and puts it in (get '<name> 'kernel) for
the local package.
Example: 
 (defkernel kron ((:consumes tangle_1 tangle_2)
 		  (:produces tangle_out)
 		  (:controls tag_out)
		  (:parameters (size_2 int))
		  (:depends ((tangle_1 tangle_items)
		 	     (tangle_2 tangle_items)
			     (size_2 int))
			   \"...\"))
                 \"...\")
Make sure the formal parameters in the kernel
  consumes/proces/... match those of the :depends clause, because
  their actual parameters will be shared.
"
  (with-gensyms (g-kernel)
    (labels
	((build-parameter-list (args)
	   `(list ,@(loop for (name type) in args
			  collect `(make-formal-parameter :name ',name 
							  :type ',type))))
	 (handle-property-clause (property args)
	   (case property
	     (:consumes `(setf (kernel-consumes ,g-kernel) ',args))
	     (:produces `(setf (kernel-produces ,g-kernel) ',args))
	     (:controls `(setf (kernel-controls ,g-kernel) ',args))
	     (:parameters 
	      `(setf (kernel-parameters ,g-kernel)
		     ,(build-parameter-list args)))
	     (:depends 
	      `(setf (kernel-tuner ,g-kernel)
		     (make-cnc-step-tuner 
		      :depends-body ,(second args)
		      :parameters ,(build-parameter-list (first args))))))))
      `(let ((,g-kernel (make-kernel :name ',name
				     :body ,kernel-body)))
	 ,@(loop for (property . args) in clauses
		 collect (handle-property-clause property args))
	 (setf (get ',name (ensure-symbol 'kernel))
		,g-kernel)
	 nil))))

(defstruct generator-function
  name
  kernel
  actual-parameters)

(defun match-parameter-values (formal-parameters parameter-bindings)
  "return the values of the formal-parameters, taking values out of
the matching parameter-bindings"
  (loop with bindings = (pairlis (mapcar #'actual-parameter-name
					 parameter-bindings)
				 (mapcar #'actual-parameter-value
					 parameter-bindings))
	for param in formal-parameters
	for binding = (assoc (formal-parameter-name param)
			     bindings)
	when binding
	  collect (cdr binding)))

(defun cnc-program-prescriptions (program)
  "Returns a list of (tag.step) pairs representing the prescriptions in
the given cnc-program"
  (loop for tag in (cnc-program-tags program)
	append (loop for step in (cnc-tag-collection-prescribes tag) 
		     collect (cons (cnc-tag-collection-name tag)
				   (cnc-step-collection-name step)))))

(defun cnc-program-produces (program)
  "Returns a list of (step.item) pairs representing
  the produce relations in the given cnc-program"
  (loop for step in (cnc-program-steps program)
	append (loop for item in (cnc-step-collection-produces step)
		     collect (cons (cnc-step-collection-name step)
				   (cnc-item-collection-name item))))
)

(defun cnc-program-consumes (program)
  (loop for step in (cnc-program-steps program)
	append (loop for item in (cnc-step-collection-consumes step)
		      collect (cons (cnc-step-collection-name step)
				    (cnc-item-collection-name item)))))

(defun cnc-program-controls (program)
  (loop for step in (cnc-program-steps program)
	append (loop for tag in (cnc-step-collection-controls step)
		     collect (cons (cnc-step-collection-name step)
				   (cnc-tag-collection-name tag)))))




;;;; PRINT GRAPH TO GRAPHVIZ ;;;;

(defun show-dot (program)
  (with-open-file (dotfile
		   #P"/tmp/graph.dot" 
		   :direction :output 
		   :if-does-not-exist :create 
		   :if-exists :supersede)
    (format dotfile "digraph {~%" )
    (loop for step in (cnc-program-steps program)
	  do (format dotfile "\"~A\" [shape=ellipse];~%" 
		     (cnc-step-collection-name step)))
    (loop for item in (cnc-program-items program)
	  do (format dotfile "\"~A\" [shape=rectangle];~%" 
		     (cnc-item-collection-name item)))
    (loop for tag in (cnc-program-tags program)
	  do (format dotfile "\"~A\" [shape=trapezium];~%" 
		     (cnc-tag-collection-name tag)))
    (loop for (tag . step) in (cnc-program-prescriptions program)
	  do (format dotfile  "\"~A\" -> \"~A\" [style=dotted];~%"
		     tag step))
    (loop for (step . item) in (cnc-program-consumes program)
	  do (format dotfile  "\"~A\" -> \"~A\" [];~%"
		     item step))
    (loop for (step . item) in (cnc-program-produces program)
	  do (format dotfile  "\"~A\" -> \"~A\" [];~%"
		     step item))
    (loop for (step . tag) in (cnc-program-controls program)
	  do (format dotfile  "\"~A\" -> \"~A\" [];~%"
		     step tag))
    (format dotfile "}~%" )
    )
  (format t "Dumped dot file in /tmp/graph.dot~%")
					; (sb-ext:run-program "/Applications/Graphviz.app/Contents/MacOS/Graphviz" (list "/tmp/graph.dot"))
  )

