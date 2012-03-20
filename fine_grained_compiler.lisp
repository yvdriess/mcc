(defpackage :fine-grained-mcc
  (:nicknames :fgc)
  (:use :cl :mcg)
  ;(:import-from alexandria if-let)
  ;(:export compile-to-cnc)
  )

(defstruct instruction 
  operand
  destinations)

(defstruct df-program
  input-ports
  output-ports
  program)

(defun compile-dataflow-program (mc-graph)
  (let ((df-program (make-df-program))
	(output-tangles (mcg::output-tangles mc-graph)))
    (dolist (tangle output-tangles)
      (compile-))

)
  )