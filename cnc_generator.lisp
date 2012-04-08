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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (speed 0) (debug 3) (safety 3))))

(defpackage :cnc-generator
  (:nicknames :cnc-gen)
  (:use :cl :cnc)
  (:import-from alexandria with-gensyms)
  (:export build))

(in-package :cnc-gen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *MIT-license*
   "
/*
 Copyright (C) 2011 by Yves Vandriessche

 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the \"Software\"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in
 all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 THE SOFTWARE.
*/
")

  (defvar *header-preamble*
"

#ifndef INCLUDED_MCCOMPILED_H
#define INCLUDED_MCCOMPILED_H

#include <complex>
#include <stdlib.h>
#include <unistd.h>
#include <new>
#include <memory>

#include <cnc/cnc.h>
#include <cnc/debug.h>
#include <cnc/default_tuner.h>


typedef std::complex<double> amplitude;

typedef CnC::item_collection<int, amplitude> tangle_items_type;
typedef CnC::tag_collection<int> tangle_tags_type;

inline std::ostream & cnc_format( std::ostream& os, const amplitude& amp )
{
    os << \" ( \" << amp.real() << \" + \" << amp.imag() << \"i\" << \" )\";
    return os;
}

struct context;

template<class collection, size_t size>
class collection_array {
public:
  collection_array() { /* very unsafe */ }
  collection_array(context& c) {
        char str[16];
	collection *ptr = static_cast<collection*>( (void*)raw );
	for( int i = 0; i < size; ++i ) {
          sprintf(str,\"col_%d\",i);
	  new( &ptr[i] )collection( c, str );
	}
  }
  collection& operator[]( size_t i ) const {
    return static_cast<collection*>( (void*)raw )[i]; 
  }
private:
  char raw[size*sizeof(collection)];
};

")
 
 
  (defvar *indentation* 0)
  (defvar *seperator* "")
  (defvar *line-stream* t)
  (defvar *header-name*)

(defun symbol-to-string (sym)
  (string-downcase (symbol-name sym)))
(defun symbol-list-to-string (symlist)
  (mapcar #'symbol-to-string symlist))
)

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

(defun body-block (string)
  (let ((line-end (position #\Newline string)))
    (unless (zerop (length string))
      (line (subseq string 0 line-end))
      (when line-end
	  (body-block (subseq string (1+ line-end)))))))

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

;;;;;;;;;;;;;;;;;;;
;;;; UTILITIES ;;;;
;;;;;;;;;;;;;;;;;;;

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



(defun generate-ctor-and-members (name formal-parameters)
  (let ((parameter-names (mapcar #'cnc::formal-parameter-name 
				 formal-parameters))
	(parameter-types (mapcar #'cnc::formal-parameter-type 
				 formal-parameters))
	)
    ;; data members
    (lines "~A ~A;" parameter-types parameter-names)
    ;; constructor
    (line "~A(" name)
    (indented 
      (comma-seperated
	(lines "~A _~A" parameter-types parameter-names)
	(line "): ")
	(indented
	  (comma-seperated 
	    (lines "~A(_~:*~A)" parameter-names))
	  (line "{};"))))))

(defun getcount-tuner-name (getcount)
  (format nil "tuner_get_~d" getcount))

(defun generate-item-tuners (program)
  (line "
struct tangle_tuner : public CnC::hashmap_tuner
{
    const int count;
    tangle_tuner(int count_): count(count_) {}
    // provide number gets to each item
    int get_count( const int & tag ) const { return count; }
};
")
  ;; assuming everything is a hashmap tuner, at the moment
  (loop for tuner in (mapcar #'cnc-item-collection-tuner
			     (cnc-program-items program))
	with getcounts
	when tuner
	  do (pushnew (cnc::cnc-item-tuner-get-count tuner)
		      getcounts)
	finally 
	   (loop for N in getcounts
		 do (line "tangle_tuner ~A( ~d );"
			  (getcount-tuner-name N)
			  N))))



#|
(:consumes tangle_1 tangle_2)
(:produces tangle_out)
(:controls tag_out)
(:parameters (size_2 int)) 
|#

(defun make-dependency-parameters (kernel)
    (declare (type kernel kernel))
  (append (loop for dependency-name in (append (kernel-consumes kernel)
					       (kernel-produces kernel)
					       )
		collect (make-formal-parameter 
			 :name dependency-name
			 :type "tangle_items_type&"))
	  (loop for dependency-name in (kernel-controls kernel)
		collect (make-formal-parameter :name dependency-name
					       :type "tangle_tags_type&"))))

(defun dependency-index-name (name)
  (format nil "index_to_~A" name))


#+nil(defun generate-tuners (program)
  ;; step tuners, actuals will come from cnc-item-collection's actual parameters
  (loop for step-tuner in (mapcar #'kernel-tuner 
				  (distinct-kernels program))
	when step-tuner
	do (with-accessors ((name cnc::cnc-step-tuner-name) 
			    (body cnc::cnc-step-tuner-depends-body) 
			    (parameters cnc::cnc-step-tuner-parameters)
			    (deriving-from cnc::cnc-step-tuner-deriving-from)) step-tuner
	     (line "struct ~A : public ~A {" name deriving-from)
	     (indented 
	       (loop for par-name in parameters
		     when (match-formal-parameter ))
	      (generate-ctor-and-members name
					 parameters)
	      (line "template< class dependency_consumer >")
	      (line "void depends( const int& t, context& c, dependency_consumer& dC ) const {")
	      (indented (line body))
	      (line "}"))
	     (line "};")
	   )))

#+nil(defun constify-parameters (formal-parameters)
  (mapcar #'(lambda (param)
	      (setf (cnc::formal-parameter-type param)
		    (format nil "const ~A" (cnc::formal-parameter-type
					    param)))
	      param)
	  formal-parameters))

(defun marshal-parameters (kernel)
  (append (make-dependency-parameters kernel) 
	  (kernel-parameters kernel)
	  #+nil(constify-parameters (kernel-parameters kernel))))

(defun generate-step-header (kernel)
  (let ((name (format nil "~A" (operation-type kernel))))
    (line "~%struct ~A: public CnC::step_tuner<> {" 
	  name
	  #+nil(when (kernel-tuner kernel) 
	    (cnc::cnc-step-tuner-deriving-from (kernel-tuner kernel))))
    (indented
      (generate-ctor-and-members name
				 (marshal-parameters kernel))
      (line "int execute( const int& t, context& c ) const;")
      (line "")
      (line "~A& operator=( const ~A& obj ) {"
	    (operation-type kernel)
	    (operation-type kernel))
      (indented 
	(line "return (*this = ~A(obj));" (operation-type kernel)))
      (line "}")
      (line "")
      (when (kernel-tuner kernel)
	;; can possibly also make this an item tuner and add get_count
	;; or max
	(line "template< class dependency_consumer >")
	(line "void depends( const int& t, context& c, dependency_consumer& dC ) const {")
	(indented
	  (body-block (cnc::cnc-step-tuner-depends-body (kernel-tuner kernel))))
	(line "}"))))
    (line "};"))

(defun generate-step-headers (program)
  (let ((kernels (distinct-kernels program)))
    (loop for kernel in kernels
	  do (generate-step-header kernel))))

(defun step-type (obj)
  (format nil "CnC::step_collection<~A, ~:*~A>"
	  (operation-type obj)))

(defmethod operation-type ((step cnc-step-collection))
  (format nil "operation_~A" 
	  (kernel-name (cnc-step-collection-kernel step))))
(defmethod operation-type ((kernel kernel))
  (format nil "operation_~A" 
	  (kernel-name kernel)))
(defmethod operation-type ((name symbol))
  (format nil "operation_~A" name))
(defun operation-store-name (kernel)
  (format nil "~A_objects" (kernel-name kernel)))
(defun step-store-name (kernel)
  (format nil "~A_step_objects" (kernel-name kernel)))

(defun generate-context-header (program)
  (line "~%struct context: public CnC::context< context > {~%")
  (indented    
    (loop for kernel in (distinct-kernels program)
	  for step-type = (step-type kernel)
	  do (line "std::vector< ~A > ~A;" 
		   (operation-type kernel)
		   (operation-store-name kernel))
	  do (line "std::vector< ~A* > ~A;"
		   (step-type kernel)
		   (step-store-name kernel)))
    (line "collection_array<tangle_items_type, ~A> items;"
	  (length (cnc-program-items program)))
    (line "collection_array<tangle_tags_type, ~A> tags;"
	  (length (cnc-program-tags program)))
    (line  "context();"))
  (line "};~%"))

(defun generate-source-sink-functions (program)
  (let ((source-kernel (cnc::cnc-program-source-kernel program))
	(sink-kernel (cnc::cnc-program-sink-kernel program)))
    ;; todo fill in signature from kernel consumes/params
    (line "~%void ~A(tangle_items_type& out_items, tangle_tags_type& out_tags, int size) {" 
	  (cnc::kernel-name source-kernel))
    (indented
      (body-block (cnc::kernel-body source-kernel)))
    (line "}~%")
    (line "~%void ~A(tangle_items_type& in_items, int size) {"
	  (cnc::kernel-name sink-kernel))
    (indented
      (body-block (cnc::kernel-body sink-kernel)))
    (line "}~%")))

(defun generate-header (program)
  ;; preamble
  (line #.*MIT-license*)
  (line #.*header-preamble*)
  ;(generate-tuners program)
  (generate-step-headers program)
  (newline)
  ;; declare/define pervasive functions such as permute
  (lines "~A" (cnc::cnc-program-utility-function-bodies program))
  (generate-source-sink-functions program)
  (generate-context-header program)
  (line "#endif"))


;;;; SOURCE FILE GENERATORS ;;;;

(defun generate-source (program)
  (line #.*MIT-license*)
  (line "#include <stdio.h>")
  (line "#include <stdlib.h>")
  (line "#include <math.h>")
  (line "#include \"~A\"~%" *header-name*)
  (generate-main-source program)
  (generate-item-tuners program)
  (generate-context-constructor-source program)
  (generate-step-source program))

(defun generate-main-source (program)
  (let ()
    (line "int main(int argc, char* argv[]) {")
    (indented
      (line "opterr = 0;")
      (line "int debug_level=0;")
      (line "int threads=0;")
      (line "int scheduler_stats=0;")
      (line "int c; 
while ((c = getopt (argc, argv, \"dist:\")) != -1)
  switch (c) {
    case 't':
      threads = atoi(optarg);
      break;
    case 's':
      scheduler_stats=1;
      break;
    case 'd':
      debug_level = 1;
      break;
    case '?':
          if (optopt == 't')
               fprintf (stderr, \"Option -%c requires an argument.\\n\", optopt);
             else if (isprint (optopt))
               fprintf (stderr, \"Unknown option `-%c'.\\n\", optopt);
             else
               fprintf (stderr,
                        \"Unknown option character `\\\\x%x'.\\n\",
                        optopt);
             return 1;
           default:
             abort ();
  }")
      
      (line "if (threads>0) { CnC::debug::set_num_threads( threads ); }")

      (line "context ctx;")

      (line "if (debug_level) { ")
      (indented
	(line "// doesn't work on CnC 0.7 anymore")
	(line "//CnC::debug::trace_all(ctx, \"context\");")
	(line "for( int i(0); i<~d; ++i)"
	      (length (cnc-program-items program)))
	(indented 
	  (line "CnC::debug::trace( ctx.items[i] );"))
	(line "for( int i(0); i<~d; ++i)"
	      (length (cnc-program-tags program)))
	(indented 
	  (line "CnC::debug::trace( ctx.tags[i] );"))
	(line "// not tracing steps for now, waiting for trace_all to get fixed")
	#+nil(loop for step in (cnc-program-steps program)
	      do (line "CnC::debug::trace( ctx.~A );"
		       (cnc-step-collection-name step))))
      (line "}~%")
      (line "if( scheduler_stats )")
      (line "  CnC::debug::collect_scheduler_statistics(ctx);")

      ;; start timer
      (line "const tbb::tick_count start_counter( tbb::tick_count::now() );")

      ;; insert code that fills the right tag and item collections with elements
      (generate-source-calls program)
     
      (line "ctx.wait();")

      (line "const tbb::tick_count::interval_t timer( tbb::tick_count::now() - start_counter );")
      (line "fprintf(stderr,\"%f\",timer.seconds());")

      ;; insert code that retrieves the info
      (generate-sink-calls program)
      
      (line "return CnC::CNC_Success;"))
    (line "}~%")))


#+nil(
 (:consumes tangle_1 tangle_2)
 (:produces tangle_out)
 (:controls tag_out)
 (:parameters (size_2 int)))

(defun verify-dependencies-with-kernel-p (step kernel)
  (and (= (length (kernel-consumes kernel))
	  (length (cnc-step-collection-consumes step)))
       (= (length (kernel-produces kernel))
	  (length (cnc-step-collection-produces step)))
       (= (length (kernel-controls kernel))
	  (length (cnc-step-collection-controls step)))))

(defmethod get-coll-ref ((coll cnc-item-collection) 
			 (program cnc-program))
  (format nil "items[~d]" 
	  (position coll (cnc-program-items program))))

(defmethod get-coll-ref ((coll cnc-tag-collection) 
			 (program cnc-program))
  (format nil "tags[~d]" 
	  (position coll (cnc-program-tags program))))

(defun marshal-bindings-for-kernel (bindings kernel)
  "destructively sorts the bindings in the order of the formal parameters"
  (let ((formal-params (marshal-parameters kernel)))
    (assert (= (length bindings) (length formal-params)))
    (loop for param in formal-params
	  for param-pos from 0
	  for pos = (position (cnc::formal-parameter-name param)
			      bindings :key #'car)
	  do (rotatef (nth pos bindings)
		      (nth param-pos bindings))))
  (the list bindings))

(defun all-step-bindings (step program)
  "Creates a list of bindings, a cons cell with param name in car and
value in cdr."
  (let ((kernel (cnc-step-collection-kernel step)))
    (assert (verify-dependencies-with-kernel-p step kernel))
    (pairlis (append (kernel-consumes kernel) 
		     (kernel-produces kernel)
		     (kernel-controls kernel))
	     (mapcar #'(lambda (coll) (get-coll-ref coll program))
		     (append (cnc-step-collection-consumes step)
			     (cnc-step-collection-produces step)
			     (cnc-step-collection-controls step)))
	     (mapcar #'(lambda (binding) 
			 (cons (actual-parameter-name binding) 
			       (actual-parameter-value binding)))
		     (cnc-step-collection-parameter-bindings step)))))

(defun match-bindings (param-names bindings)
  (loop for name in param-names
	for match = (assoc name bindings)
	when match
	  collect match))

#+nil(defun generate-step-tuner-instances (program)
  ;; possible TODO, avoid creating redundant tuner instances
  (loop for step in (cnc-program-steps program)	
	for tuner = (kernel-tuner (cnc-step-collection-kernel step))
	when tuner
	  do (let ((bindings (match-bindings (cnc::cnc-step-tuner-parameters tuner)
					     (all-step-bindings step
								program)))
		   (tuner-instance-name (format nil "tuner_~A" (gensym ""))))
	       (line "~A ~A(~{~A~^, ~});"
		     (cnc::cnc-step-tuner-name tuner)
		     tuner-instance-name
		     (mapcar #'cdr bindings))
	       (setf (cnc::cnc-step-collection-tuner-instance-name step)
		     tuner-instance-name))))

(defun times-consumed (item)
  (let ((tuner (cnc::cnc-item-collection-tuner item)))
    ;; no tuner means no consuming steps
    ;; in turn this means only consumed once, by the sink function
    (if tuner
	(cnc::cnc-item-tuner-get-count tuner)
	1)))

(defun operation-instance (step program)
  (let ((bindings (all-step-bindings step program)))
    ;; using the order with which the step kernel ctor was defined:
    ;; consumes, produces, controls and then parameters
    (format nil "~A(~{~A~^,~})"
	    (operation-type step)
	    (mapcar #'cdr 
		    (marshal-bindings-for-kernel bindings
						 (cnc-step-collection-kernel step)))
	    #+nil(append
		  (mapcar #'item-index (cnc-step-collection-consumes step))
		  (mapcar #'item-index (cnc-step-collection-produces step))
		  (mapcar #'tag-index (cnc-step-collection-controls step))
		  (mapcar #'actual-parameter-value 
			  (cnc-step-collection-parameter-bindings step))))))

(defun prescribed-by (step program)
  (let* ((prescriptions (cnc-program-prescriptions program))
	 (pair (rassoc (cnc-step-collection-name step) 
		       prescriptions)))
    (assert pair)
    (find (car pair) (cnc-program-tags program) 
	  :key #'cnc-tag-collection-name)))


(defun generate-context-constructor-source (program)
  (line "context::context(): ")
  (indented 
   (line "CnC::context< context >(),")
   ;; instantiate step collection data members
   #+nil(lines "~A( *this , \"~:*~A\", ~A~@[, ~A~])," 
	  (step-names program)
	  (mapcar #'(lambda (step) 
		      (generate-step-instance step program))
		  (cnc::cnc-program-steps program))
	  (mapcar #'cnc::cnc-step-collection-tuner-instance-name
		  (cnc::cnc-program-steps program)))
;   (line "items(*this),")
   (line "tags(*this)")
   (indented
    (line "{")
    (indented
     (let (;(prescriptions (cnc-program-prescriptions program))
	   ;(consumes (cnc-program-consumes program))
	   ;(produces (cnc-program-produces program)) (format
	   ;(controls (cnc-program-controls program))
	   )
       ;; TODO would be interesting to see difference between unrolled
       ;; and for loop version
       (loop for item in (cnc-program-items program)
	     for index from 0
	     when (times-consumed item)
	       do (line 
		   "new(&items[~d])tangle_items_type(*this, \"~A\", ~A);"
			index
			(cnc-item-collection-name item)
			(getcount-tuner-name (times-consumed item)))
		  #+nil(line "new(&items[~d])tangle_items_type(*this, \"~A\");"
			index
			(cnc-item-collection-name item)))
       (loop for step in (cnc-program-steps program)
	     for kernel = (cnc-step-collection-kernel step)
	     for produces = (cnc-step-collection-produces step)
	     for consumes = (cnc-step-collection-consumes step)
	     for controls = (cnc-step-collection-controls step)
	     do (line "~A.push_back( ~A );"
		      (operation-store-name kernel)
		      (operation-instance step program)) 
	     do (line "~A.push_back( new ~A(*this, \"~A\", ~A.back(), ~:*~A.back()) );" 
		      (step-store-name kernel)
		      (step-type kernel)
		      (cnc-step-collection-name step)
		      (operation-store-name kernel))
	     do (line  "~A.prescribes(*~A.back(), *this);"
		       (get-coll-ref (prescribed-by step program)
				     program)
		       (step-store-name kernel))
	     do (loop for item in consumes
		      for item-ref = (get-coll-ref item program)
		      do (line "~A_step_objects.back()->consumes( ~A );" 
			       (kernel-name kernel)
			       item-ref))
	     do (loop for item in produces
		      for item-ref = (get-coll-ref item program)
		      do (line "~A_step_objects.back()->produces( ~A );"
			       (kernel-name kernel)
				item-ref))
	     do (loop for tag in controls
		      for tag-ref = (get-coll-ref tag program)
		      do (line "~A_step_objects.back()->controls( ~A );"
			       (kernel-name kernel)
			       tag-ref))
)

       #+nil(loop for tag in (cnc-program-tags program)
	     for index from 0
	     do (loop for step in (cnc-tag-collection-prescribes tag)
		      for step-name = (cnc-step-collection-name step)
		      do (line "tags[~d].prescribes(~A, *this);" 
			       index
			       step-name)))
))
    (line "}~%"))))


#+nil(defun make-dependency-parameters (kernel)
       (declare (type kernel kernel))
       (append (loop for dependency-name in (append (kernel-consumes kernel)
						    (kernel-produces kernel)
						    (kernel-controls kernel))
		     collect (make-formal-parameter :name dependency-name
						    :type "size_t"))))


(defun generate-step-source (program)
  ;; generate each step's body
  (loop for kernel in (distinct-kernels program)
	do (progn 
	     (line "int ~A::execute(const int & t, context & c ) const {" 
		   (operation-type kernel))
	     (indented
	      (body-block (kernel-body kernel))
	      (newline)
	      (line "return CnC::CNC_Success;"))
	     #+nil(indented 
	       (lines "tangle_items_type& ~A(c.items[~A]);"
		      (cnc::kernel-consumes kernel)
		      (mapcar #'dependency-index-name
			      (cnc::kernel-consumes kernel)))
	       (lines "tangle_items_type& ~A(c.items[~A]);"
		      (cnc::kernel-produces kernel)
		      (mapcar #'dependency-index-name
			      (cnc::kernel-produces kernel)))
	       (lines "tangle_tags_type& ~A(c.tags[~A]);"
		      (cnc::kernel-controls kernel)
		      (mapcar #'dependency-index-name
			      (cnc::kernel-controls kernel)))
	       (body-block (cnc::kernel-body kernel))
	       (newline)
	       (line "return CnC::CNC_Success;"))
	     (line "}~%"))))

(defun generate-source-calls (program)
  (loop for item-collection in
	     (cnc::cnc-program-input-item-collections program)
	for tag-collection = 
	     (cnc::cnc-item-collection-associated-tags item-collection)
	do (line "~A( ctx.~A, ctx.~A, ~d );"
		 (cnc::kernel-name (cnc::cnc-program-source-kernel
				    program))
		 (get-coll-ref item-collection program)
		 (get-coll-ref tag-collection  program)
		 (cnc::cnc-item-collection-size item-collection))))

(defun generate-sink-calls (program)
  (loop for item-collection in
	     (cnc::cnc-program-output-item-collections program)
	do (line "~A( ctx.~A, ~d );"
		 (cnc::kernel-name (cnc::cnc-program-sink-kernel
				    program))
		 (get-coll-ref item-collection program)
		 (cnc::cnc-item-collection-size item-collection))))

;;;;;;;;;;;;;;;
;;;; BUILD ;;;;
;;;;;;;;;;;;;;;

(defun build (program
	      &key (target-directory "") 
	           (target-filename "mccompiled"))
  "Entry point for code generation; call with an cnc-program object
	      representing the cnc program to be generated."
  (declare (type cnc::cnc-program program))
  (let ((header (concatenate 'string target-directory target-filename ".h"))
	(source (concatenate 'string target-directory target-filename ".C"))
	(*print-case* :downcase))
    (format t "Generating header file... ")
    (write-to-file header
      (generate-header program))
    (format t "done~%Generating source file...")
    (let ((*header-name* (concatenate 'string target-filename ".h")))
     (write-to-file source (generate-source program)))
    (format t "done~%Written to ~A and ~A in ~A.~%"  header source (directory target-directory))))


