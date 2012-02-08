(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *indentation* 0)
  (defvar *seperator* "")
  (defvar *line-stream* t)
  (defvar *filename* "stresstest")

  (defun symbol-to-string (sym)
    (string-downcase (symbol-name sym)))
  (defun symbol-list-to-string (symlist)
    (mapcar #'symbol-to-string symlist)))

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

(defvar *header*)
(setq *header* "
#ifndef INCLUDED_STRESSTEST_H
#define INCLUDED_STRESSTEST_H

#include <complex>
#include <stdlib.h>
#include <unistd.h>

#include <cnc/cnc.h>
#include <cnc/debug.h>

struct context;
")

(defun generate-step-header (step-name)
  (line "~%struct ~A {" step-name)
  (indented 
    (line "int execute( const int& t, context& c ) const;"))
  (line "};"))

(defun generate-context-header (item-names tag-names prescriptions)
  (line "~%struct context: public CnC::context< context > {~%")
  (indented
    (lines "CnC::item_collection< int, int > ~A;" item-names)
    (line "CnC::item_collection< int, bool > signals;")
    (lines "CnC::tag_collection< int > ~A;" tag-names)
    (line "context(): ")
    (indented 
      (line "CnC::context< context >(),")
      (lines "~A( this )," item-names)
      (comma-seperated
	(lines "~A( this , false )" tag-names))
      (indented
	(line "{")
	(indented
	  (loop for entry in prescriptions
		do (line "prescribe( ~A , ~A() );" (cdr entry) (car entry)))
	  )
	(line "}~%"))))
  (line "};~%"))


(defun generate-main-source (input-tag-names)
  (line "int main(int argc, char* argv[]) {")
  (indented
    (line "const int elements = argc>1 ? argv[1] : 32")
    (line "context ctx;")
    ;; insert code that fills the right tag and item collections with elements
    (lines "ctx.~A.put(elements);" input-tag-names)

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

(defun generate-source (step-names step-bodies input-tag-names)
  (line "#include <stdio.h>")
  (line "#include <stdlib.h>")
  (line "#include \"~A.h\"~%" *filename*)
  (generate-main-source input-tag-names)
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

(defun generate-header (item-names 
			tag-names 
			step-names
			prescriptions
)
  ;; preamble
  (line #.*header*)
  (mapc #'generate-step-header step-names)
  (newline)
  (generate-context-header item-names tag-names prescriptions)
  (line "#endif"))

(defun build (item-names tag-names step-names step-bodies 
	      input-tag-names prescriptions
	      &key (target-directory "") 
	           (target-header-file "stresstest.h") 
	           (target-source-file "stresstest.C"))
  (let ((header (concatenate 'string target-directory target-header-file))
	(source (concatenate 'string target-directory target-source-file)))
    (format t "Generating header file... ")
    (write-to-file header
      (generate-header item-names 
		       tag-names 
		       step-names
		       prescriptions))
    (format t "done~%Generating source file... ")
    (write-to-file source
      (generate-source step-names step-bodies input-tag-names))
    (format t "done~%Written to ~A and ~A.~%" header source)))

(defun step-body (n)
  (format nil "
int i = t;
int data;
c.items_~d.get(i, data);
c.items_~d.put(i, ++data);
c.tags_~d.put(i);
" 
	  n (1+ n) (1+ n))
)

(defun source-body (size)
  (format nil "
for(int i=0;i<~d; ++i) {
  c.items_1.put(i,1);
  c.tags_1.put(i);
}
"
	  size))

(defun sink-body (last size)
  (format nil "
int results[~d];
for(int i=0;i<~d; ++i) {
  c.items_~d.get(i,results[i]);
  printf(\"results[%d]: %d\\n\",i,results[i]);
}
"
	  size
	  size
	  last))

(defun generate-depth-stresstest (depth)
  (let ((size 16)
	(item-names    (loop for i from 1 to depth
			     collect (format nil "items_~d" i)))
	(tag-names     (loop for i from 1 to depth
			     collect (format nil "tags_~d" i)))
	(step-names    (loop for i from 1 below depth
			     collect (format nil "step_~d" i)))
	(step-bodies   (loop for i from 1 below depth
			     collect (step-body i)))
	(prescriptions (loop for i from 1 below depth
			     collect (cons (format nil "step_~d" i)
					   (format nil "tags_~d" i)))))
    (build item-names
	   tag-names
	   (append step-names
		   (list "source" "sink"))
	   (append step-bodies
		   (list (source-body size)
			 (sink-body depth size)))
	   (list "source")
	   (append prescriptions
		   (list (cons "sink" 
			       (format nil "tags_~d" depth)))))
))

(generate-depth-stresstest 3)