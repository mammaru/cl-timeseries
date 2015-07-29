(in-package :clts)

(defclass vector-auto-regressive-model ()
  (dimention
   :accessor dimention
   :initarg :dimention
   :initform 1)
  (A ;;transition matrix
   :accessor A
   :initarg :A
   :initform 1)
  (E ;;error matrix
   :accessor E
   :initarg :E
   :initform 1)) 

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
   :initform :x0var)
  )
