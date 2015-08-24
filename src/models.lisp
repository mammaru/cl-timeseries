(in-package :clts-user)

;;
;; Classes
;;
(defclass dataframe ()
  (data
   :initarg :data
   :initform "must be specified data."
   :accessor data
   :documentation "a set of observed data")
  (index
   :accessor index
   :documentation "number of data points")
  (dimension
   :accessor dimension
   ::documentation "dimension of each data"))

(defclass time-series-model ()
  ((parameters
	:initarg :data
	:accessor data
	:documentation "time series observation data")
   (transision
	:initarg :dimension
	:type integer
	:reader dimension
	:documentation "dimension of each step observation for time series model")))

(defclass vector-auto-regressive-model (time-series-model)
  ((dimension
	:initarg :dimension
	:initform (error "Must be specified dimension of observation at each time points by :dimension")
	:accessor dimension)
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


(defun make-linear-transition (a e)
  #'(lambda (x)
	  (progn
		(setf x (M+ (M* a x) (multivariate-normal e)))
		x)))

(defun make-time-series (initial-values transition-function)
  (let ((v initial-values))
	#'(lambda ()
		(setf v (transition-function v)) )))

(defmacro define-transition (init)
  `())

(defmacro define-time-series-model (name (&rest superclasses))
  `(defclass ,name ))






;; junk scripts
(defparameter *tmp* (zeros 10 10))
(M* *tmp* (rand 10 10))
*tmp*
(transpose-matrix *tmp*)
(check-type *tmp* matrix-like)
;(assert (= (nrows *tmp*) (ncols *tmp*)))
(cholesky-decomposition *tmp*)



(defmacro make-transition-closure (name x0 &body body)
  `(let ((x ,x0))
	 (setf ,name
	   #'(lambda ()
		   (progn
			 (setf x ,@body)
			 x)))))
  
(defun make-linear-transition (a e)
  #'(lambda (x)
	  (progn
		(setf x (M+ (M* a x) (multivariate-normal e)))
		x)))

(defclass time-series-model ()
  ((parameters
	:initarg :params
	:accessor parameters
	:documentation "parameters of time series model")
   (transision
	:initarg :dimension
	:type function
	:reader transition
	:documentation "closure that specify transition of each step") ))

(defstruct parameters
  initial-value
  transition-matrix
  error-variance)

(defclass vector-auto-regressive-model (time-series-model)
  ((dimension
	:initarg :dimension
	:initform (error "Must be specified dimension of observation")
	:accessor dimension)
   (parameters
	:initform (make-parameters))
   (transition))

(defmethod initialize-instance :after ((model vector-auto-regressive-model) &key)
  (with-slots ((dim dimension) (params parameters)) model
	(with-slots ((x0 initial-value)
				 (A transition-matrix)
				 (sigma error-variance)) params
	  (setf A (rand dim dim))
	  (setf sigma (eye dim dim))
	  (setf (slot-value model 'transition)
			(let ((x x0))
			  (lambda ()
				(progn
				  (setf x (M+ (M* A x) (multivariate-normal sigma)))
				  x) ))))))

(defmethod transition ((model vector-auto-regressive-model))
  (with-slots (transition) model
	(transition ()) ))
