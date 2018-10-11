;;;; cl-markov-chains.lisp

(in-package #:cl-markov-chains)

(defun analyze (data order)
  (loop :with ht := (make-hash-table :test 'equalp)
	:for read-pointer :from 0 :below (- (length data) order)
	:for key := (subseq data read-pointer (+ read-pointer order))
        :for next := (elt data (+ read-pointer order))
	:do (if (not (hash-table-p (gethash key ht)))
		(progn
		  (setf (gethash key ht) (make-hash-table))
		  (setf (gethash next (gethash key ht)) 1))
		(if (not (gethash next (gethash key ht)))
		    (setf (gethash next (gethash key ht)) 1)
		    (incf (gethash next (gethash key ht)))))
	:finally (return ht)))

(defun next-state (ht current)
  (alexandria:when-let ((current-ht (gethash current ht)))
    (let* ((hash-size (hash-table-count current-ht))
	   (all-keys (alexandria:hash-table-keys current-ht))
	   (all-values (alexandria:hash-table-values current-ht))
	   (value-total (reduce #'+ all-values))
	   (prob-array (make-array hash-size
				   :initial-contents (mapcar (lambda (x)
							       (coerce (/ x value-total)
								       'double-float))
							     all-values)))
	   (values-array (make-array hash-size
				     :initial-contents all-keys))
	   (rp (make-discrete-random-var prob-array :values values-array)))
      (funcall rp))))

(defun any-key (ht)
  (loop :for r :being :the :hash-keys :of ht
	:return r))

(defun generate (ht size &optional
			   (order (length (any-key ht)))
			   (initial-elt (any-key ht)))
  (typecase initial-elt
    (vector (let ((results (make-array (length initial-elt)
				       :initial-contents initial-elt
				       :adjustable t
				       :element-type (array-element-type initial-elt)
				       :fill-pointer (length initial-elt))))
	      (loop :repeat (1- size)
		    :for next := (next-state ht (subseq results
						      (- (fill-pointer results)
							 order)
						      (fill-pointer results)))
		    :while next
		    :do (vector-push-extend next results))
	      results))
    (t
     (let ((results (reverse initial-elt)))
       (loop :repeat (1- size)
	     :for next := (next-state ht (reverse (subseq results 0 order)))
	     :while next
	     :do (push next results))
       (reverse results)))))
