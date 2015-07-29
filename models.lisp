(in-package :clts-user)

(defclass vector-auto-regressive-model ()
  ((dimention
   :initarg :dimention
   :initform 1
   :accessor dimention
   :documentation "dimention of vaiables")
  (transition-matrix
   :initarg :transition-matrix
   :initform 1
   :accessor A
   :documentation "transition matrix or coefficient")
  (E
   :initarg :error-matrix
   :initform 1
   :accessor E
   :documentation "variance matrix of error")))

(defclass state-space-model ()
  ((observation-dimention
   :initarg :obs-dim
   :initform (error "Must supply dimention of observation as :obs-dim"))
  (system-dimention
   :initarg :sys-dim
   :initform (error "Must supply dimention of system as :sys-dim"))
  (initial-mean-of-system
   :initarg :x0mean)
  (initial-variance-of-system
   :initarg :x0var)))
