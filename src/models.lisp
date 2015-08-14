(in-package :clts-user)

;;
;; Classes
;;
(defclass time-series-model ()
  ((observation
	:initarg :observation
	:accessor observation
	:documentation "time series observation data")
   (dimension
	:initarg :dimension
	:type integer
	:accessor dimension
	:documentation "dimension of each step observation for time series model")))

(defclass vector-auto-regressive-model (time-series-model)
  ((dimension
	:initarg :dimension
	:initform (error "Must be specified dimension of observation at each time points by :dimension"))
   (transition-matrix
	:initarg :A
	:accessor A
	:documentation "transition matrix or coefficient")
   (error-mean
	:initarg :mu
	:documentation "mean of error")
   (error-variance-matrix
	:initarg :sigma
	:accessor sigma
	:documentation "variance matrix of error")
   (values
	:reader v
	:documentation "generated values by vector-auto-regressive-model")))

(defclass auto-regressive-model (vector-auto-regressive-model)
  ((coefficient
	:initarg :coefficient
	:documentation "coefficient of regression at each step")
   (dimension
	:reader dimension
	:initform 1)
   (time-points
	:initarg :time-points
	:documentation "total number of time-series data")))

(defclass state-space-model (time-series-model)
  ((dimension
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

;;
;; Methods
;;
(defmethod initialize-instance :after ((model vector-auto-regressive-model) &key)
  (let ((dim (slot-value model 'dimension)))
	(setf (slot-value model 'transition-matrix) (rand dim dim))
	(setf (slot-value model 'error-mean) (zeros dim 1))
	(setf (slot-value model 'error-variance-matrix) (eye dim dim))
	(setf (slot-value model 'values) (list (multivariate-normal (eye dim dim))))))

;; TODO: closure is better?
(defgeneric transition (model)
  (:documentation "one-step-transition of each time series model."))

(defmethod transition ((model vector-auto-regressive-model))
  (with-slots ((dim dimension) (A transition-matrix) (E error-matrix) (v values)) model
	  (let ((past-value (last v)))
		(setf v (cons (last v) (M+ (M* A past-value) (multivariate-normal E)))) )))

(defmethod transition ((model state-space-model))
  (with-slots (dim dimension) model
	))

(defmethod sparse-vector-auto-regression ((model vector-auto-regressive-model))
  (with-slots (dim dimension) model
	))






;; scripts
(defparameter *tmp* (zeros 10 10))
(M* *tmp* (rand 10 10))
*tmp*
(transpose-matrix *tmp*)
(check-type *tmp* matrix-like)
;(assert (= (nrows *tmp*) (ncols *tmp*)))
(cholesky-decomposition *tmp*)
