(in-package :clts-user)

(defclass vector-auto-regressive-model ()
  ((dimention
	:initarg :dimention
	:initform 1
	:accessor dimention
	:documentation "dimention of vaiables")
   (transition-matrix
	:initarg :A
	:initform 1
	:accessor A
	:documentation "transition matrix or coefficient")
   (error-matrix
	:initarg :E
	:initform 1
	:accessor E
	:documentation "variance matrix of error")
   (values
	:initform (list (rand dimention 1)))))

(defclass state-space-model ()
  ((observation-dimention
	:initarg :obs-dim
	:initform (error "Must supply dimention of observation as :obs-dim"))
   (system-dimention
	:initarg :sys-dim
	:initform (error "Must supply dimention of system as :sys-dim"))
   (initial-mean-of-system
	:initarg :x0mean
	:initform (rand dimention 0))
   (initial-variance-of-system
	:initarg :x0var)
   (system-values
	:initform 0)
   (observation-values
	:initform 0)))

(defun cholesky-decomposition (matrix)
  (let ((tmp matrix) (dim-row (nrows matrix)) (dim-col (ncols matrix)))
	(case dim-row
	  (dim-col (let ((dim dim-col))
				 (loop
					for i from 0 to dim
					for j from i to dim
					  do ())))
	  (t (error "Augument must be square matrix")))))

(defun multivariate-normal (sigma &opptional mu)
  )

(defmethod transition ((model vector-auto-regressive-model))
  (with-slots (dim dimention) model
	(with-slots (A transition-matrix) model
	  (with-slots (E error-matrix) model
		(with-slots (values values) model
			(let ((past-value (last values)))
			  (setf values (cons (last values) (M+ (M* A past-value) (multivariate-normal E))))))))))

(defmethod transition ((model state-space-model))
  (with-slots (dim dimention) model
	))

(defmethod sparse-vector-auto-regression ((model vector-auto-regressive-model))
  (with-slots (dim dimention) model
	))


;; test
(defparameter *tmp* (zeros 10 10))
(M* tmp (ones 10 1))
*tmp*
