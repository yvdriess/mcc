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

#include <cnc/cnc.h>
#include <cnc/debug.h>
#include <cnc/default_tuner.h>


typedef std::complex<double> amplitude;

typedef CnC::item_collection<int, amplitude> tangle_items_type;
typedef CnC::tag_collection<int> tangle_tags_type;

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
    (lines "const ~A ~A;" parameter-types parameter-names)
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

(defun generate-item-tuners (program)
  (declare (ignore program))
  (line "
struct tangle_tuner : public CnC::hashmap_tuner
{
    const int count;
    tangle_tuner(int count_): count(count_) {}
    // provide number gets to each item
    int get_count( const int & tag ) const { return count; }
};
")
  (line "tangle_tuner tuner_get_one( 1 );")
  (line "tangle_tuner tuner_get_two( 2 );")
  ;; assuming everything is a hashmap tuner, at the moment
  #+nil(loop for tuner in (cnc-program-items program)
	with getcounts
	when tuner
	  do (pushnew (cnc::cnc-item-tuner-get-count tuner)
		      getcounts)
	finally 
	   (loop for N in getcounts
		 do (line "tangle_tuner<~d> item_tune_getcount_~d;"))))

#+nil(defun generate-tuners (program)
  ;; step tuners, actuals will come from cnc-item-collection's actual parameters
  (loop for step-tuner in (mapcar #'cnc::kernel-tuner 
				  (distinct-kernels program))
	when step-tuner
	do (with-accessors ((name cnc::cnc-step-tuner-name) 
			    (body cnc::cnc-step-tuner-depends-body) 
			    (parameters cnc::cnc-step-tuner-parameters)
			    (deriving-from cnc::cnc-step-tuner-deriving-from)) step-tuner
	     (line "struct ~A : public ~A {" name deriving-from)
	     (indented 
	      (generate-ctor-and-members name 
					 consumes
					 produces
					 parameters)
	      (line "template< class dependency_consumer >")
	      (line "void depends( const int& t, context& c, dependency_consumer& dC ) const {")
	      (indented (line body))
	      (line "}"))
	     (line "}")
	   )))


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
					       (kernel-controls kernel))
		collect (make-formal-parameter 
			 :name (dependency-index-name dependency-name)
			 :type "size_t"))))

(defun dependency-index-name (name)
  (format nil "index_to_~A" name))


(defun generate-step-header (kernel)
  (let ((name (format nil "step_~A" (kernel-name kernel))))
    (line "~%struct ~A {" name)
    (indented
      (generate-ctor-and-members name
				 (append (make-dependency-parameters kernel) 
					 (kernel-parameters kernel)))
      (line "int execute( const int& t, context& c ) const;"))
    (line "};")))

(defun generate-step-headers (program)
  (let ((kernels (distinct-kernels program)))
    (loop for kernel in kernels
	  do (generate-step-header kernel))))

(defun generate-context-header (program)
  (line "~%struct context: public CnC::context< context > {~%")
  (indented    
   (lines "CnC::step_collection< step_~A > ~A;" 
	  (step-kernel-names program)
	  (step-names program))
   (line "collection_array< tangle_items_type, ~A> items;" 
	 (length (cnc::cnc-program-items program)))
   (line "collection_array< tangle_tags_type, ~A> tags;"
	 (length (cnc::cnc-program-tags program)))
   (line  "context();")
   )
  (line "};~%"))

(defun generate-source-sink-functions (program)
  (let ((source-kernel (cnc::cnc-program-source-kernel program))
	(sink-kernel (cnc::cnc-program-sink-kernel program)))
    ;; todo fill in signature from kernel consumes/params
    (line "~%void ~A(tangle_items_type& out_items, tangle_tags_type& out_tags, int size) {" 
	  (cnc::kernel-name source-kernel))
    (indented
      (line (cnc::kernel-body source-kernel)))
    (line "}~%")
    (line "~%void ~A(tangle_items_type& in_items, int size) {"
	  (cnc::kernel-name sink-kernel))
    (indented
      (line (cnc::kernel-body sink-kernel)))
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

      ;; make tuner variables:  tune_`kernel_name` `step_name`(actual params);

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
	(loop for step in (cnc-program-steps program)
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

(defun get-item-index (item program) 
  (declare (type cnc-item-collection item)
	   (type cnc-program program))
  (position item (cnc-program-items program)))

(defun get-tag-index (tag program)
  (declare (type cnc-tag-collection tag)
	   (type cnc-program program))
  (position tag (cnc-program-tags program)))

(defun generate-step-instance (step program)
  "Genererates step collection instantiation code, calling its ctor"
  (let ((kernel (cnc-step-collection-kernel step)))
    (assert (verify-dependencies-with-kernel-p step kernel))
    ;; using the order with which the step kernel ctor was defined:
    ;; consumes, produces, controls and then parameters
    (flet ((item-index (item) (get-item-index item program))
	   (tag-index  (tag) (get-tag-index  tag  program)))
      (format nil "step_~A(~{~A~^,~})"
	      (cnc::kernel-name (cnc-step-collection-kernel step))
	      (append
	       (mapcar #'item-index (cnc-step-collection-consumes step))
	       (mapcar #'item-index (cnc-step-collection-produces step))
	       (mapcar #'tag-index (cnc-step-collection-controls step))
	       (mapcar #'actual-parameter-value 
		       (cnc-step-collection-parameter-bindings step)))))))

(defun generate-tuner-instance (step)
  (with-slots (kernel
	       parameter-bindings) step
    (with-slots (name parameters) (cnc::kernel-tuner kernel)
      (format nil "~A(~{~A~^,~})"
	      name
	      ;; cheating a bit, pass all the consumes item
	      ;; collections and the matching parameters
	      (append (kernel-consumes kernel)
		      (match-parameter-values parameters
					      parameter-bindings))))))


(defun times-consumed (item)
  (let ((tuner (cnc::cnc-item-collection-tuner item)))
    ;; no tuner means no consuming steps
    ;; in turn this means only consumed once, by the sink function
    (if tuner
	(cnc::cnc-item-tuner-get-count tuner)
	1)))



(defun generate-context-constructor-source (program)
  (line "context::context(): ")
  (indented 
   (line "CnC::context< context >(),")
   ;; instantiate step collection data members
   (lines "~A( *this , \"~:*~A\", ~A)," 
	  (step-names program)
	  (mapcar #'(lambda (step) (generate-step-instance step program))
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
		   "new(&items[~d])tangle_items_type(*this, 
  \"~A\"~[, tuner_get_one~;, tuner_get_two~:;, tangle_tuner(~:*~d)~]);"
			index
			(cnc-item-collection-name item)
			(1- (times-consumed item)))
		  #+nil(line "new(&items[~d])tangle_items_type(*this, \"~A\");"
			index
			(cnc-item-collection-name item)))
       (loop for tag in (cnc-program-tags program)
	     for index from 0
	     do (loop for step in (cnc-tag-collection-prescribes tag)
		      for step-name = (cnc-step-collection-name step)
		      do (line "tags[~d].prescribes(~A, *this);" 
			       index
			       step-name)))
       (loop for step in (cnc-program-steps program)
	     for step-name = (cnc-step-collection-name step)
	     for produces = (cnc-step-collection-produces step)
	     for consumes = (cnc-step-collection-consumes step)
	     for controls = (cnc-step-collection-controls step)
	     do (loop for item in consumes
		      for item-index = (get-item-index item program)
		      do (line "~A.consumes( items[~d] );" 
			       step-name
			       item-index))
	     do (loop for item in produces
		      for item-index = (get-item-index item program)
		      do (line "~A.produces( items[~d] );"
				step-name
				item-index))
	     do (loop for tag in controls
		      for tag-index = (get-tag-index tag program)
		      do (line "~A.controls( tags[~d] );"
			       step-name
			       tag-index)))))
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
	     (line "int step_~A::execute(const int & t, context & c ) const {" 
		   (cnc::kernel-name kernel))
	     (indented 
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
	       (line (cnc::kernel-body kernel))
	       (newline)
	       (line "return CnC::CNC_Success;"))
	     (line "}~%"))))

(defun generate-source-calls (program)
  (loop for item-collection in
	     (cnc::cnc-program-input-item-collections program)
	for tag-collection = 
	     (cnc::cnc-item-collection-associated-tags item-collection)
	do (line "~A(ctx.items[~d], ctx.tags[~d], ~d);"
		 (cnc::kernel-name (cnc::cnc-program-source-kernel
				    program))
		 (get-item-index item-collection program)
		 (get-tag-index  tag-collection  program)
		 (cnc::cnc-item-collection-size item-collection))))

(defun generate-sink-calls (program)
  (loop for item-collection in
	     (cnc::cnc-program-output-item-collections program)
	do (line "~A(ctx.items[~d], ~d);"
		 (cnc::kernel-name (cnc::cnc-program-sink-kernel
				    program))
		 (get-item-index item-collection program)
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


