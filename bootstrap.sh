#!/bin/bash
sbcl --load cnc_generator.lisp --load graph.lisp --eval "(save-lisp-and-die \"mcc\" :executable t :toplevel #'mcc::mc-read-compile)"
