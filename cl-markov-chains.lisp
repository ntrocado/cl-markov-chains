;;;; cl-markov-chains.lisp

(in-package #:cl-markov-chains)

(defun analyze (data order)
  (loop :with ht := (make-hash-table :test 'equalp)
	:for read-pointer :from 0 :below (- (length data) order)
	:for key := (subseq data read-pointer (+ read-pointer order))
        :for next := (elt data (+ read-pointer order))
	:for hash-key := (gethash key ht)
	:do (if (not (hash-table-p hash-key))
		(progn
		  (setf (gethash key ht) (make-hash-table))
		  (setf (gethash next (gethash key ht)) 1))
		(if (not (gethash next hash-key))
		    (setf (gethash next hash-key) 1)
		    (incf (gethash next hash-key))))
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

(defun ht->matrix (ht)
  "Return a transition matrix for the model in HT and a list with the designators for each state. Only works with first-order chains."
  (assert (= (length (any-key ht)) 1) nil "HT must be a first-order chain.")
  (values 
   (make-array
    (list (hash-table-count ht) (hash-table-count ht))
    :initial-contents (loop
			:for x :being :the :hash-keys :of ht
			:collect
			(loop
			  :with sum-values := (reduce #'+
						      (alexandria:hash-table-values
						       (gethash x ht)))
			  :for y :being :the :hash-keys :of ht
			  :for v := (gethash (elt y 0) (gethash x ht))
			  :collect (if (not v)
				       0
				       (/ v sum-values)))))
   (alexandria:hash-table-keys ht)))

(defun define-transition (ht previous next probability)
  (when (not (hash-table-p (gethash (alexandria:ensure-list previous) ht)))
    (setf (gethash (alexandria:ensure-list previous) ht) (make-hash-table)))
  (setf (gethash next (gethash (alexandria:ensure-list previous) ht))
	probability)
  ht)

(defun define-model (transition-list)
  "Return a nested hash-table for the model defined by a list of transistions. TRANSITION-LIST must be in the foloowing form:
'((previous-state1 next-state1 probability1) 
  (previous-state2 next-state2 probability2) 
  ... 
  (previous-state-n next-state-n probability-n)."
  (let ((ht (make-hash-table :test 'equal)))
    (mapc (alexandria:curry #'apply #'define-transition ht) transition-list)
    ht))

(defun define-model-from-matrix (elements transition-matrix)
  "Return a nested hash table for the model defined by a transition matrix for
elements. Matrix in the form (elements from left to right and top to bottom):
  '(.9   .05  .01  .04)
  '(.05  .9   .01  .04)
  '(0    0    .9   .1 )
  '(0    0    0    1  )"
  (let (transition-list (row-count 0) (column-count 0) )
    (dolist (element elements)
      (setf column-count 0)
      (dolist (other elements)
        (push (list element other (nth column-count
                                       (nth row-count transition-matrix)))
              transition-list)
        (setf column-count (1+ column-count)))
      (setf row-count (1+ row-count)))
    (define-model (nreverse transition-list))))
