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
  (:use :cl)
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
  (defvar *indentation* 0)
  (defvar *seperator* "")
  (defvar *line-stream* t)
  (defvar *filename* "mccompiled")

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
  "Returns T if any of the supplied arg-lists contained an empty argument listj, nil (nil))"
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
()  `(progn (terpri *line-stream*)
	  *line-stream*))

;; (defun generate-start-input (tag-collection)
;;   ;; this tag collection only needs 1 element to bootstrap the content generation
;;   (line "void start-input-~A() {" tag-collection)
;;   (indented
;;     (line "ctx.~A.put(0);" tag-collection))
;;   (line "};")
;;   )


(defun declare-tuners ()
  (line "
struct context;

struct kron_tuner : public CnC::default_tuner< int, context >
{
   kron_tuner(const unsigned int size2,
              CnC::item_collection< int, amplitude > &input): size2(size2), input(input) {}
   const unsigned int size2;
   CnC::item_collection< int, amplitude > &input;

  template< class dependency_consumer >
    void depends( const int & tag, context & c, dependency_consumer & dC ) const;
};

struct m_tuner : public CnC::default_tuner< int, context >
{
   m_tuner(const unsigned int size,
           const unsigned int qid,
           CnC::item_collection< int, amplitude > &input): 
        size(size), qid(qid), input(input) {}
   const unsigned int size;
   const unsigned int qid;
   CnC::item_collection< int, amplitude > &input;

  template< class dependency_consumer >
    void depends( const int & tag, context & c, dependency_consumer & dC ) const;
};
"))

(defun define-tuners ()
  (line "
template< class dependency_consumer >
void kron_tuner::depends( const int & tag, context & c, dependency_consumer & dC ) const
{
  for(unsigned int i(0);i<size2;++i) {
    dC.depends( input , i );
  }
}

template< class dependency_consumer >
void m_tuner::depends( const int & tag, context & c, dependency_consumer & dC ) const
{
  if ( (tag & qid) == 0 ) {
    dC.depends( input , tag );
    dC.depends( input , tag + qid);
  }
}
"))


(defun generate-step-header (step-name)
  (line "~%struct ~A {" step-name)
  (indented 
    (line "int execute( const int& t, context& c ) const;"))
  (line "};"))

(defun generate-context-header (item-names tag-names prescriptions tuned-steps item-sizes)
  (line "~%struct context: public CnC::context< context > {~%")
  (indented
    (lines "CnC::item_collection< int, amplitude > ~A; // SIZE=~A" item-names item-sizes)
    (line "CnC::item_collection< int, bool > signals;")
    (lines "CnC::tag_collection< int > ~A;" tag-names)
    (line "context(): ")
    (indented 
      (line "CnC::context< context >(),")
      (lines "~A( this )," item-names)
      (line "signals( this ),")
      (comma-seperated
	(lines "~A( this , false )" tag-names))
      (indented
	(line "{")
	(indented
	  (loop for entry in prescriptions
	     for tuned = (assoc (car entry) tuned-steps)
	     if tuned
							   ; this is
							   ; hidious!
	       do (line "prescribe( ~A , ~A() , ~A(~{ ~A ~^,~}) );" 
			(cdr entry) (car entry) (cadr tuned) (cddr tuned))
	     else do (line "prescribe( ~A , ~A() );" (cdr entry) (car entry)))
	  #+nil(lines "prescribe( ~A, ~A() );"
		 (mapcar #'cdr prescriptions)
		 (mapcar #'car prescriptions))
;	  (lines "~A.put(0);" input-tag-names)
	  )
	(line "}~%"))))
  (line "};~%"))

(defun generate-header (item-names 
			tag-names 
			step-names
			prescriptions
			tuned-steps
			item-sizes)
  ;; preamble
  (line #.*MIT-license*)
  (line #.*header-preamble*)
  (mapc #'generate-step-header step-names)
  (newline)
  (line *source-permute-function*)
  (line *source-tensor-permute-function*)
  (line *source-compact-index-function*)
  (declare-tuners)
  (generate-context-header item-names tag-names prescriptions tuned-steps item-sizes)
  (define-tuners)
  (line "#endif"))

(defun generate-main-source (item-names step-names input-tag-names)
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

    ;; debug
;      (line "CnC::debug::trace_all(ctx, \"context\");")

    (line "if (debug_level) { ")
      (indented
    	(line "CnC::debug::collect_scheduler_statistics(ctx);")
	(line "CnC::debug::trace( ctx.signals, \"signals\" );")
    	(lines "CnC::debug::trace( ~A(), \"~A\" );" step-names step-names)
    	(lines "CnC::debug::trace( ctx.~A, \"~A\");" item-names item-names))
    (line "}~%")
 
    ;; insert code that fills the right tag and item collections with elements
    (lines "ctx.~A.put(0);" input-tag-names)

    (line "ctx.wait();")
    ;; insert code that retrieves the info
    (line "return CnC::CNC_Success;"))
  (line "}~%"))

(defun generate-step-source (step-name step-body)
  (line "int ~A::execute(const int & t, context & c ) const {" step-name)
  ;; insert step logic here
  (indented 
    (line step-body)
    (line "return CnC::CNC_Success;"))
  (line "}~%"))

(defun generate-source (step-names step-bodies item-names input-tag-names)
  (line #.*MIT-license*)
  (line "#include <stdio.h>")
  (line "#include <stdlib.h>")
  (line "#include \"~A.h\"~%" *filename*)
  (generate-main-source item-names step-names input-tag-names)
;  (line *source-permute-function*)
;  (line *source-tensor-permute-function*)
  (mapcar #'generate-step-source step-names step-bodies)
  )

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




;; (defun apply-step-operation (step-operation &rest actual-parameters)
;;   (let ((step (make-step-application :name (step-operation-name step-operation)
;; 				     :formal-parameters (step-operation-formal-parameters step-operation)
;; 				     :body (step-operation-body step-operation))))
;;     (assert (= (list-length (step-operation-formal-parameters step)) 
;; 	       (list-length actual-parameters)))
;;     (setf (step-application-arguments step) actual-parameters)
;;     step))

;; (let ((cz-step (generate-cnc-step "CZ" '("tangle_1") '("tangle_2" "tag_do_on_2") ))
;;       (check_step (make-step-operation :name "check"
;; 				       :formal-parameters (list ))))

;; (let ((steps (list (apply-kernel (get 'cz 'kernel)
;; 				 '("TANGLE_1")
;; 				 '("TANGLE_2" "TAG_DO_ON_TANGLE_2")
;; 				 (mapcar #'number-to-string '(8 2 4))))))
;;   (write-to-file (concatenate 'string "~/dev/cnc/test/" *filename* ".h") 
;;     (generate-header '(tangle_1 tangle_2)
;;   		     '(tag_do_on_tangle_1 tag_do_on_tangle_2)
;; 		     steps))
;;   (write-to-file (concatenate 'string "~/dev/cnc/test/" *filename* ".C")
;;     (generate-source steps)))

(defun build (item-names tag-names step-names step-bodies 
	      input-tag-names prescriptions tuned-steps item-sizes
	      &key (target-directory "") 
	           (target-header-file "mccompiled.h") 
	           (target-source-file "mccompiled.C"))
  (let ((header (concatenate 'string target-directory target-header-file))
	(source (concatenate 'string target-directory target-source-file)))
    (format t "Generating header file... ")
    (write-to-file header
      (generate-header item-names 
		       tag-names 
		       step-names
		       prescriptions
		       tuned-steps
		       item-sizes))
    (format t "done~%Generating source file... ")
    (write-to-file source
      (generate-source step-names step-bodies item-names input-tag-names))
    (format t "done~%Written to ~A and ~A.~%" header source)))
