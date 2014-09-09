#!/bin/bash
sbcl --eval '(load "mccompiler.asd")' \
     --eval '(ql:quickload :mccompiler)' \
     --eval "(save-lisp-and-die \"mcc\" :executable t :toplevel #'mcc::compile-to-cnc)"
