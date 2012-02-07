(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *indentation* 0)
  (defvar *seperator* "")
  (defvar *line-stream* t)
  (defvar *filename* "mccompiled")

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

(defun generate-context-header (item-names tag-names prescriptions tuned-steps)
  (line "~%struct context: public CnC::context< context > {~%")
  (indented
    (lines "CnC::item_collection< int, amplitude > ~A; // SIZE=~A" item-names)
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
