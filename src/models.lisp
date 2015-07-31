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
  (let ((dim (square-matrix-p m)))
	(if dim
		(loop
		   for i from 0 to dim
		   for j from i to dim
		   summing (Mref m i j) into s)
		(error "Augument must be square matrix"))))

(defun multivariate-normal (sigma &optional mu)
  )

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
(M* tmp (ones 10 1))
*tmp*
(transpose-matrix *tmp*)
(check-type *tmp* matrix-like)
;(assert (= (nrows *tmp*) (ncols *tmp*)))
