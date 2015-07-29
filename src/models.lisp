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

(defun multivariate-normal (variance-matrix)
  )

(defmethod transition ((model vector-auto-regressive-model))
  (with-slots (dim dimention) model
	(with-slots (A transition-matrix) model
	  (with-slots (E error-matrix) model
		(with-slots (values values)
			(let ((last-value (last values)))
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
