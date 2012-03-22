#!/bin/bash
sbcl --load quicklisp.lisp \
     --eval "(ql:quickload :mccompiler)" \
     --eval "(save-lisp-and-die \"mcc\" :executable t :toplevel #'mcc::compile-to-cnc)"
