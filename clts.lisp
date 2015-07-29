(in-package :clts-user)

(defclass vector-auto-regressive-model ()
  (dimention
   :accessor dimention
   :initarg :dimention
   :initform 1
   :documentation "dimention of vaiables")
  (A
   :accessor A
   :initarg :A
   :initform 1
   :documentation "transition matrix or coefficient")
  (E ;;error matrix
   :accessor E
   :initarg :E
   :initform 1
   :documentation "variance matrix of error")) 

(defclass state-space-model ()
  (observation-dimention
   :initarg :obs-dim
   :initform (error "Must supply dimention of observation as :obs-dim"))
  (system-dimention
   :initarg :sys-dim
   :initform (error "Must supply dimention of system as :sys-dim"))
  (initial-mean-of-system
   :initarg :x0mean)
  (initial-variance-of-system
   :initform :x0var))
