;;;; cl-markov-chains.asd

(asdf:defsystem #:cl-markov-chains
  :description "A library for modeling Markov chains in Common Lisp."
  :author "Nuno Trocado <http://nunotrocado.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("alexandria")
  :components ((:file "package")
               (:file "cl-markov-chains")
	       (:file "alias_method_v2")))
