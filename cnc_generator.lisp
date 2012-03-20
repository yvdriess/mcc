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
  (ql:quickload 'alexandria)
  (declaim (optimize (speed 0) (debug 3) (safety 3))))

(defpackage :cnc-generator
  (:nicknames :cnc-gen)
  (:use :cl :cnc)
  (:import-from alexandria compose mappend with-gensyms)
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

#include <cnc/cnc.h>
#include <cnc/debug.h>

typedef std::complex<double> amplitude;

struct context;
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
				 formal-parameters)))
    ;; data members
    (lines "~A ~A;" parameter-types parameter-names)
    ;; constructor
    (line "~A(" name)
    (indented 
     (comma-seperated
      (lines " ~A _~A" parameter-types parameter-names)
      (line "): ")
      (indented
       (comma-seperated 
	(lines "~A(_~:*~A)" parameter-names))
       (line "{};"))))))

(defun generate-tuners (program)
  ;; step tuners, actuals will come from cnc-item-collection's actual parameters
  (loop for step-tuner in (mapcar #'cnc::kernel-tuner 
				  (distinct-kernels program))
	do (with-slots (name body parameters deriving-from) step-tuner
	     (line "struct ~A : public ~S {" name deriving-from)
	     (indented 
	      (generate-ctor-and-members name parameters)
	      (line "template< class dependency_consumer >")
	      (line "void depends( const int& t, fib_context& c, dependency_consumer& dC ) const {")
	      (indented (line body))
	      (line "}"))
	     (line "}")
	   )))

(defun generate-step-header (kernel)
  (let* ((name (format nil "step_~A" (cnc::kernel-name kernel)))
	 (parameters (cnc::kernel-parameters kernel)))
    (line "~%struct ~A {" name)
    (indented
     (generate-ctor-and-members name parameters)
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
   (lines "CnC::item_collection< int, ~A > ~A;" 
	  (item-names program)
	  (item-types program))
   (lines "CnC::tag_collection< int > ~A;" 
	  (tag-names program))
   (line  "context();")
   )
  (line "};~%"))

(defun generate-utility-headers ())

(defun generate-header (program)
  ;; preamble
  (line #.*MIT-license*)
  (line #.*header-preamble*)
  (generate-tuners program)
  (generate-step-headers program)
  (newline)
  ;; declare/define pervasive functions such as permute
  (lines "~S" (cnc::cnc-program-utility-function-bodies program))
  (generate-context-header program)
  (line "#endif"))


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
	(source (concatenate 'string target-directory target-filename ".C")))
    (format t "Generating header file... ")
    (write-to-file header
      (generate-header program))
    (format t "done~%Generating source file... ")
    #+nil(let ((*header-name* (concatenate 'string target-filename ".h")))
     (write-to-file source
       (generate-source program)))
    (format t "done~%Written to ~A and ~A.~%" header source)))


;;;; SOURCE FILE GENERATORS ;;;;

(defun generate-source (program)
  (line #.*MIT-license*)
  (line "#include <stdio.h>")
  (line "#include <stdlib.h>")
  (line "#include <math.h>")
  (line "#include \"~A\"~%" *header-name*)
  (generate-main-source program)
  (generate-context-constructor-source program)
  (generate-step-source program)
  
  )

(defun generate-main-source (program)
  (let ()
    (line "int main(int argc, char* argv[]) {")
    (indented
      (line "opterr = 0;")
      (line "int debug_level=0;")
      (line "int threads=0;")
      (line "int c; 
while ((c = getopt (argc, argv, \"dt:\")) != -1)
  switch (c) {
    case 't':
      threads = atoi(optarg);
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
	(line "CnC::debug::trace_all(ctx, \"context\");")
	(line "CnC::debug::collect_scheduler_statistics(ctx);")
	)
      (line "}~%")

      ;; insert code that fills the right tag and item collections with elements
      (generate-source-calls program)

      (line "CnC::debug::init_timer();")
      (line "ctx.wait();")
      (line "CnC::debug::finalize_timer(\"-\");")

      ;; insert code that retrieves the info
      (generate-sink-calls program)
      
      (line "return CnC::CNC_Success;"))
    (line "}~%")))

(defun generate-step-instance (step)
  (with-slots (name 
	       kernel
	       parameter-bindings) step 
    (format nil "step_~A(~{~A~^,~})"
	    (cnc::kernel-name kernel)
	    (mapcar #'cnc::actual-parameter-value parameter-bindings))))


(defun generate-tuner-instance (step)
  (with-slots (kernel
	       parameter-bindings) step
    (with-slots (name parameters) (cnc::kernel-tuner kernel)
      (format nil "~A(~{~A~^,~})"
	      name
	      ;; cheating a bit, pass all the consumes item
	      ;; collections and the matching parameters
	      (append (cnc::kernel-consumes kernel)
		      (match-parameter-values parameters
					      parameter-bindings)))))
  )


(defun generate-context-constructor-source (program)
  (line "context::context(): ")
    (indented 
      (line "CnC::context< context >(),")
      ;; instantiate step collection data members
      (lines "~A( *this , \"~:*~A\", ~A)," 
	     (step-names program)
	     (mapcar #'generate-step-instance (cnc::cnc-program-steps program))
;	     (mapcar #'generate-tuner-instance (cnc::cnc-program-steps
;program))
	     )
      (lines "~A( *this , \"~:*~A\" )" (item-names program))
      (lines "~A( *this , \"~:*~A\" )" (tag-names program))
      (indented
	(line "{")
	(indented
	  (let ((prescriptions (cnc-program-prescriptions program))
		(consumes (cnc-program-consumes program))
		(produces (cnc-program-produces program))
		(controls (cnc-program-controls program)))
	   (lines "~A.prescribes( ~A, *this );"
		  (mapcar #'car prescriptions)
		  (mapcar #'cdr prescriptions))
	    (lines "~A.consumes( ~A );"
		   (mapcar #'car consumes)
		   (mapcar #'cdr consumes))
	    (lines "~A.produces( ~A );"
		   (mapcar #'car produces)
		   (mapcar #'cdr produces))
	    (lines "~A.controls( ~A );"
		   (mapcar #'car controls)
		   (mapcar #'cdr controls))))
	(line "}~%"))))


(defun generate-step-source (program)
  ;; generate each step's body
  (loop for kernel in (distinct-kernels program)
	do (progn 
	     (line "int step_~A::execute(const int & t, context & c ) const {" 
		   (cnc::kernel-name kernel))
	     (indented 
	       (let ((formals (mapcar #'cnc::kernel-parameters kernel)))
		 (lines "~A ~A;" 
			(mapcar #'cnc::formal-parameter-type formals)
			(mapcar #'cnc::formal-parameter-name formals)))
	       (line (cnc::kernel-body kernel))
	       (newline)
	       (line "return CnC::CNC_Success;"))
	     (line "}~%"))))

(defun generate-source-calls (program)
  (loop for item-collection in
	     (cnc::cnc-program-input-item-collections program)
	for tag-collection = 
	     (cnc::cnc-item-collection-associated-tags item-collection)
	do (line "~A(ctx.~A, ctx.~A, ~d);"
		 (cnc::kernel-name (cnc::cnc-program-source-kernel
				    program))
		 (cnc::cnc-item-collection-name item-collection)
		 (cnc::cnc-tag-collection-name tag-collection)
		 (cnc::cnc-item-collection-size item-collection))))

(defun generate-sink-calls (program)
  (loop for item-collection in
	     (cnc::cnc-program-output-item-collections program)
	do (line "~A(ctx.~A, ~d);"
		 (cnc::kernel-name (cnc::cnc-program-sink-kernel
				    program))
		 (cnc::cnc-item-collection-name item-collection)
		 (cnc::cnc-item-collection-size item-collection))))
