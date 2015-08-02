(in-package :clts-user)

(defclass vector-auto-regressive-model ()
  ((dimension
	:initarg :dimension
	:initform 1
	:accessor dimension
	:documentation "dimension of vaiables")
   (transition-matrix
	:initarg :A
	:initform (rand dimension dimension)
	:accessor A
	:documentation "transition matrix or coefficient")
   (error-matrix
	:initarg :E
	:initform (eye dimension dimension)
	:accessor E
	:documentation "variance matrix of error")
   (values
	:initform (list (rand dimension 1)))))

(defclass state-space-model ()
  ((observation-dimension
	:initarg :obs-dim
	:initform (error "Must supply dimension of observation as :obs-dim"))
   (system-dimension
	:initarg :sys-dim
	:initform (error "Must supply dimension of system as :sys-dim"))
   (initial-mean-of-system
	:initarg :x0mean
	:initform (rand dimension 0))
   (initial-variance-of-system
	:initarg :x0var)
   (system-values
	:initform 0)
   (observation-values
	:initform 0)))

(defun square-matrix-p (m)
  "Checking whether matrix is square or not. If square, returns dimension of matrix."
  (check-type m matrix-like)
  (if (reduce #'= (matrix-dimensions m)) (ncols m) nil))
  ;(let ((dim-row (nrows m)) (dim-col (ncols m)))
	;(if (= (nrows m) (ncols m)) dim-row nil)))

(defun cholesky-decomposition (m)
  "Returns lower triangular matrix that squared to be original matrix."
  (let ((dim (square-matrix-p m)))
	(if dim
		(let ((L (zeros dim dim)) (s 0))
		  (dotimes (i dim)
			(do ((j 0 (1+ j)))
				((= j i))
			  ((setf s 0)
			   (do ((k 0 (1+ k)))
				   ((= k j) (setf (Mref L i j) (/ s (Mref L j j))))
				 (decf s (M* (Mref L i k) (Mref L j k))))))
			((setf s 0)
			 (do ((k 0 (1+ k)))
				 ((= k i) (setf (Mref L i i) (sqrt s)))
			   (decf s (expt (Mref L i k) 2)))))
		  L)
		(error "Augument must be square matrix"))))

(defun multivariate-normal (sigma &optional mu)
  (let ((Q cholesky-decomposition sigma)) ((z (rand (square-matrix-p sigma) 1)))
	(if mu (M+ mu (M* Q z)) (M* Q z))))

(defmethod transition ((model vector-auto-regressive-model))
  (with-slots (dim dimension) model
	(with-slots (A transition-matrix) model
	  (with-slots (E error-matrix) model
		(with-slots (v values) model
			(let ((past-value (last values)))
			  (setf values (cons (last v) (M+ (M* A past-value) (multivariate-normal E))))))))))

(defmethod transition ((model state-space-model))
  (with-slots (dim dimension) model
	))

(defmethod sparse-vector-auto-regression ((model vector-auto-regressive-model))
  (with-slots (dim dimension) model
	))






;; junk
(defparameter *tmp* (zeros 10 10))
(M* tmp (rand 10 10))
*tmp*
(transpose-matrix *tmp*)
(check-type *tmp* matrix-like)
;(assert (= (nrows *tmp*) (ncols *tmp*)))
(cholesky-decomposition *tmp*)
