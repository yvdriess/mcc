#!/bin/bash
sbcl --eval "(ql:quickload :mccompiler)" \
     --eval "(save-lisp-and-die \"mcc\" :executable t :toplevel #'mcc::compile-to-cnc)"
