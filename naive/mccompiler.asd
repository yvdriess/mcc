(in-package :asdf)
     
(defsystem "mccompiler"
  :description "measurement calculus parallelizing compiler"
  :version "0.1.2"
  :author "Yves Vandriessche <yvdriess@vub.ac.be>"
  :licence "MIT license"
  :depends-on ("alexandria")
  :components ((:file "graph") 
	       (:file "cnc_model")
	       (:file "cnc_generator" :depends-on ("cnc_model"))
	       (:file "mccompiler" :depends-on ("cnc_model" "graph" "cnc_generator"))))
