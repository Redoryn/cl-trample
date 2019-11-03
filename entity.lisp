(in-package #:cl-trample)

(defclass entity ()
  ((pos-x :initarg :x :initform 0 :accessor pos-x)
   (pos-y :initarg :y :initform 0 :accessor pos-y)
   (vel-x :initarg :vel-x :initform 0 :accessor vel-x)
   (vel-y :initarg :vel-y :initform 0 :accessor vel-y)
   (height :initarg :h :initform 1 :accessor height)
   (width :initarg :w :initform 1  :accessor width)
   (sprite :initarg :sprite :initform nil :accessor sprite)
   ;(dest-rect :initform nil :accessor dest-rect)
   (angle :initform 0.0 :accessor angle)
   ))

