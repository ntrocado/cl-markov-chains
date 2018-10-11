;;;; package.lisp

(defpackage #:cl-markov-chains
  (:use #:cl)
  (:export #:analyze
	   #:next-state
	   #:generate))
