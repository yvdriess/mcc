#!/bin/bash
sbcl --load dev/mccompiler/cnc_generator.lisp --load dev/mccompiler/graph.lisp --eval "(save-lisp-and-die \"test\" :executable t :toplevel #'mcc::mc-read-compile)"

