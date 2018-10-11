(in-package :cl-markov-chains)

;; An implementation of the Alias Method.
;;
;; The function MAKE-DISCRETE-RANDOM-VAR takes an array of
;; probabilities and an (optional) array of values. Produces a
;; function which returns each of the values with the specified
;; probability (or the corresponding integer no values have been
;; given).
;;
;; It needs only one call to RANDOM for every value produced.
;;
;; For the licence, see at the end of the file.
(defun create-alias-method-vectors (probabilities phantom-values)
  ;; Trivial queues.
  (labels ((enqueue (x q)
	     (if (car q)
		 (setf (cddr q) (list x)
		       (cdr q) (cddr q))
		 (let ((k (list x)))
		   (setf (car q) k (cdr q) k))))

	   (dequeue (q)
	     (prog1
		 (pop (car q))
	       (unless (car q) (setf (cdr q) nil)))))
			      
  (let* ((N (length probabilities))
	 (Nf (+ phantom-values N))
	 (threshold (/ 1.0d0 Nf))

	 (alternatives (make-array Nf :element-type 'fixnum))
	 
	 (p_keep (make-array N :element-type 'double-float))

	 (bigs (list nil))
	 (lows (list nil))
	 (c 0))

    
    (loop for i from 0 below phantom-values do
	  (enqueue (+ N i) lows))
    
    (map nil #'(lambda (x)
		 (if (>= threshold x)
		     (enqueue c lows)
		     (enqueue c bigs))
		 (setf (aref p_keep c) x)
		 (incf c))
	 probabilities)

    (loop :while (car lows) :do
	  (let* ((0ne (dequeue lows))
		 (Tw0 (dequeue bigs))
		 (p_keep-0ne (if (< 0ne N) (aref p_keep 0ne) 0.0d0))
		 (delta (- threshold p_keep-0ne)))

	    (if tw0
	      (progn
		  
		(setf p_keep-0ne
		      (/ p_keep-0ne threshold))
		  
		(setf (aref alternatives 0ne) Tw0)
		  
		(decf (aref p_keep Tw0) delta)
		  
		(if (>= threshold (aref p_keep Tw0))
		    (enqueue Tw0 lows)
		    (enqueue Tw0 bigs)))
	      (setf p_keep-0ne 1.0d0))

	    (when (< 0ne N)
	      (setf (aref p_keep 0ne) p_keep-0ne))))

    ;; Numerical noise might leave some bigs dangling, with
    ;; | p_keep - 1/N | very small.
    (dolist (k (car bigs))
      (setf (aref p_keep k) 1.0d0))

    (values p_keep alternatives Nf))))

(defun make-discrete-random-var (probabilities &key values (phantom-values 0))
  (when (and values (/= (length values) (length probabilities)))
    (error "different number of values and probabilities."))

  (let* ((N (length probabilities)))
    (multiple-value-bind (p_keep alternatives Nf)
	(create-alias-method-vectors probabilities phantom-values)
      #'(lambda ()
	  (labels ((result (k)
		     (if values (aref values k) k)))

	    (let ((k (random Nf)))
	      (if (>= k N) (result (aref alternatives k))
		  (let ((r (random 1.0d0)))
		    (if (> r (aref p_keep k))
			(result (aref alternatives k))
			(result k))))))))))
    
;; Tests the alias method. p holds the prescribed probabilities, and
;; cnt the measured ones.
(defun test-alias-method (n runs)
  (let ((p (make-array n :element-type 'double-float))
	(cnt (make-array n :initial-element 0.0d0)))

    (dotimes (i n)
      (setf (aref p i) (random 1.0d0)))
    
    (let ((nc (loop for i from 0 below n summing (aref p i))))

      (dotimes (i n)
	(setf (aref p i)
	      (/ (aref p i) nc)))

      (let ((rp (make-discrete-random-var p :phantom-values 100)))

	(dotimes (i runs)
	  (incf (aref cnt (funcall rp))))

	(dotimes (i n)
	  (setf (aref cnt i)
		(/ (aref cnt i) runs)))))

    (values p cnt)))

;;; Copyright (c) 2006, Mario S. Mommer <m_mommer@yahoo.com>

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
