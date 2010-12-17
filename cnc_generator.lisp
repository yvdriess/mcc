(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload 'alexandria)
  (declaim (optimize (speed 0) (debug 3) (safety 3))))

(defpackage :cnc-generator
  (:nicknames :cnc-gen)
  (:use :cl)
  (:import-from alexandria compose mappend with-gen))

(in-package :cnc-gen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *header-preamble*
"
#ifndef INCLUDED_MCCOMPILED_H
#define INCLUDED_MCCOMPILED_H

#include <cnc/cnc.h>
#include <cnc/debug.h>

typedef double amplitude;

struct context;
")
  (defvar *source-permute-function*
"
int tensor_permute(const int i,const int siz, const int i1, const int i2) {
  int o = i1;
  int m = 2;
  int n = (i1 - i2);
  int p = i2;
   return 
    i 
    + p * ( n - 1 ) * floor(i/i2)
    - p * (m*n - 1) * floor(i/m*p)
    + p * n*(n - 1) * floor(i/m*p*n);
}") 
  (defvar *indentation* 0)
  (defvar *seperator* "")
  (defvar *line-stream* t)
  (defvar *filename* "mccompiled"))

(defmacro while-writing-in ((stream) &body body)
  `(let ((*line-stream* ,stream))
     ,@body))

(defmacro indented (&body body)
  `(let ((*indentation* (1+ *indentation*)))
     ,@body))

(defmacro comma-seperated (&body body)
  `(let ((*seperator* ","))
     ,@body))

(defun insert-indent (string)
  (concatenate 'string 
	       (make-string (* 2 *indentation*) :initial-element #\ )
	       string))

(defmacro line (format-string &rest args)
  `(progn
     (format *line-stream* (insert-indent ,format-string) 
	     ,@args)
     (terpri *line-stream*)
     *line-stream*))



(defun no-more-args-p (arg-lists)
  "Returns T if any of the supplied arg-lists contained an empty argument list, nil (nil))"
  (or (null arg-lists)
      (some #'null arg-lists)
      (some #'null
	    (mapcar #'car arg-lists))))

(defun lines (format-string &rest arg-lists)
  (loop for todo-arg-lists = arg-lists then (mapcar #'cdr todo-arg-lists)      
     until (no-more-args-p todo-arg-lists)
     do (apply #'format (append (list *line-stream* (insert-indent format-string))
				      (mapcar #'car todo-arg-lists)))
     unless (no-more-args-p (mapcar #'cdr todo-arg-lists)) 
       do (format *line-stream* *seperator*)
     do (terpri *line-stream*))
  *line-stream*)

(defmacro newline ()
  `(progn (terpri *line-stream*)
	  *line-stream*))


(defun generate-header (item-collections tag-collections steps)
  ;; preamble
  (line #.*header-preamble*)
  (mapc #'generate-step-header steps)
  (newline)
  (generate-context-header item-collections tag-collections steps)
  (line "#endif"))

(defun generate-step-header (step)
  (let ((step-name (step-operation-name step))
	(formal-vars (step-operation-formal-parameters step)))
    (line "~%struct ~A {" step-name)
    (indented 
      (when formal-vars
	(lines "const int ~A;" formal-vars)
	(line "~A(~{int ~A~^,~}):" step-name formal-vars)
	(indented 
	  (indented
	    (comma-seperated
	      (lines "~A(~A)" formal-vars formal-vars)))
	  (line "{}")))
      (line "int execute( const int& t, context& c ) const;"))
    (line "};")))

(defun generate-context-header (item-names tag-names steps)
  (let ((formal-vars (mappend #'step-operation-formal-parameters steps)))
    (line "~%struct context: public CnC::context< context > {~%")
    (indented
      (lines "CnC::item_collection< int, amplitude > ~A;" item-names)
      (lines "CnC::tag_collection< int > ~A;" tag-names)
      (line "context(~{int ~A~^,~}): " formal-vars)
      (indented 
	(line "CnC::context< context >(),")
	(lines "~A( this )," item-names)
	(comma-seperated
	  (lines "~A( this , false )" tag-names))
	(indented
	  (line "{")
	  (indented
	    (lines "prescribe( ~A, ~A(~{~A~^,~}) );" 
		   tag-names 
		   (mapcar #'step-operation-name steps)
		   (mapcar #'step-operation-formal-parameters steps)))
	  (line "}~%"))))
    (line "};~%")))

(defun generate-source (steps)
  (line "#include <stdio.h>")
  (line "#include \"~A.h\"~%" *filename*)
  (generate-main-source steps)
  (line *source-permute-function*)
  (mapcar #'generate-step-source steps)
  )

(defun generate-main-source (steps)
  (let ((actual-values (mappend #'step-application-arguments steps)))
    (line "int main(int argc, char* argv[]) {")
    (indented 
      (line "context ctx(~{~A~^,~});" actual-values)
      ;; insert code that fulls the right tag and item collections with elements
      (line "ctx.wait();")
      ;; insert code that retrieves the info
      (line "return CnC::CNC_Success;"))
    (line "}~%")))

(defun generate-step-source (step)
  (let ((formal-vars (step-operation-formal-parameters step)))
    (line "int ~A::execute(const int & t, context & c ) const {" (step-operation-name step))
    ;; insert step logic here
    (indented 
      (lines "const int ~A( this->~A );" formal-vars formal-vars)
      (newline)
      (line "return CnC::CNC_Success;"))
    (line "}~%")))


(defmacro write-to-file (pathname &body body)
  "Writes all 'line' and 'lines' called in the body to the specified file path."
  `(with-open-file (out ,pathname
			:direction :output 
			:if-does-not-exist :create 
			:if-exists :supersede)
     (let ((*line-stream* out))
       ,@body)))

(defmacro writing-to-string (&body body)
  `(let ((*line-stream* (make-string-output-stream)))
     ,@body
     (get-output-stream-string *line-stream*)))

(defstruct cnc-step
  (name (gensym "step"))
  parameter-name
  parameter-value
  body)

(defun generate-cnc-step (name inputs outputs
			  parameter-names parameter-values kernel)
  (make-cnc-step :name (gensym name)
		       :parameter-names parameter-names
		       :parameter-values parameter-values
		       :body (writing-to-string 
			       (lines "const int ~A(~A);" 
				      parameter-names
				      parameter-values)
			       (funcall body-generator
					inputs
					outputs))))

(defstruct kernel 
  name
  parameter-names
  parameter-values
  (body-generator (:type function)))

(defun apply-kernel (kernel inputs outputs &optional parameter-values)
  (assert (= (length parameter-values)
	     (length (parameter-names kernel))))
  (funcall (kernel-body-generator kernel)
	   inputs 
	   outputs
	   parameter-value))

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

(defun symbol-to-string (sym)
  (string-downcase (symbol-name sym)))
(defun symbol-list-to-string (symlist)
  (mapcar #'symbol-to-string symlist))

(defun same-length-p (list-1 list-2)
  (= (list-length list-1)
     (list-length list-2)))

(defmacro defkernel ((name tag (&rest inputs) (&rest outputs) (&rest parameters))
		     body-string)
  (alexandria:with-gensyms (g-name g-tag g-inputs g-outputs g-parameters g-body-string)
    `(let  ((,g-name ,(symbol-to-string name))
	    (,g-tag ,(symbol-to-string tag))
	    (,g-inputs (list ,@(symbol-list-to-string inputs)))
	    (,g-outputs (list ,@(symbol-list-to-string outputs)))
	    (,g-parameters (list ,@(symbol-list-to-string parameters)))
	    (,g-body-string ,body-string))
       (flet ((body-generator (actual-inputs actual-outputs)
		(assert (and (same-length-p actual-inputs ,g-inputs)
			     (same-length-p actual-outputs ,g-outputs)))
		(loop 
		   for actual in (append actual-inputs actual-outputs)
		   for match in (append ,g-inputs ,g-outputs) ;we assume they are distinct
		   for replaced = (replace-all-matching (concatenate 'string "`" , "`") ,g-body-string actual)
		   then (replace-all-matching (concatenate 'string "`" match "`") replaced actual)
		   finally (return replaced))))
	 (make-kernel :name ,g-name
		      :parameter-names ,g-parameters
		      :body-generator #'body-generator)))))

(funcall (kernel-body-generator 
	  (defkernel (cz tag (in_items) (out_items out_tags) (size qid_1 qid_2))
	      "
amplitude a_i;
const int i = `tag`;
int f_i = tensor_permute( i , `size` , `qid_1`, `qid_2` );
c.`in_items`.get( f_i , a_i );
if (f_i % 4 == 3)
a_i = - a_i;
c.`out-items`.put( i , a_i );
c.`out-tags`.put( i );
"))
	 '("foo")
	 '("bar" "baz"))

(defun apply-step-operation (step-operation &rest actual-parameters)
  (let ((step (make-step-application :name (step-operation-name step-operation)
				     :formal-parameters (step-operation-formal-parameters step-operation)
				     :body (step-operation-body step-operation))))
    (assert (= (list-length (step-operation-formal-parameters step)) 
	       (list-length actual-parameters)))
    (setf (step-application-arguments step) actual-parameters)
    step))

(let ((cz-step (generate-cnc-step "CZ" '("tangle_1") '("tangle_2" "tag_do_on_2") ))
      (check_step (make-step-operation :name "check"
				       :formal-parameters (list ))))

  (write-to-file (concatenate 'string "~/dev/cnc/test/" *filename* ".h") 
    (generate-header '(tangle_1 tangle_2 tangle_3 tangle_4)
		     '(tag_do_E tag_do_M tag_3)
		     (list gen-step kron-step cz-step)))

  (write-to-file (concatenate 'string "~/dev/cnc/test/" *filename* ".C")
    (generate-source (list (apply-step-operation gen-step)
			   (apply-step-operation kron-step 2 4 8)
			   (apply-step-operation cz-step 8)))))

