(ql:quickload :cl-trample)
(in-package :cl-trample)

(defclass ui-element ()
  ((x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)
   (height :initform 0 :initarg :height :accessor height)
   (width :initform 0 :initarg :width :accessor width))
  )

(defclass frame (ui-element)
  ((parent :initform nil :initarg :parent :accessor parent)
   (children :initform nil :accessor children)))

(defclass slider (ui-element)
  ((parent :initform nil :initarg :parent :accessor parent)
   (min-value :initform 0 :initarg :min-value :accessor min-value)
   (max-value :initform 100 :initarg :max-value :accessor max-value)
   (current-value :initform 100 :initarg :current-value :accessor current-value)
   (step-size :initform 0 :initarg :step :accessor step-size)))


(defun create-frame (x y width height)
  (make-instance 'frame :x x :y y :width width :height height))



(defun create-slider (x y width height)
  (let ((s (make-instance 'slider :x x :y y :width width :height height)))
    s
    )
  )

(defun draw-border (ui-element)
  (let ((rect (sdl2:make-rect (x ui-element) (y ui-element) (width ui-element) (height ui-element))))
    (sdl2:set-render-draw-color *renderer* 0 255 0 255)
    (sdl2:render-draw-rect *renderer* rect))
  )

(defun draw-slider (slider)
  (draw-border slider)
  (let* ((top-padding 2)
	 (side-padding 7)
	 (bar-x (+ side-padding (x slider)))
	 (bar-y (floor (+ (y slider) (/ (height slider) 2))))
	 (bar-width (- (width slider) (* side-padding 2)))
	 (bar-height 2)
	 (slider-bar (sdl2:make-rect bar-x bar-y bar-width bar-height))
	 
	 (bob-width (floor (* 0.1 bar-width)))
	 (bob-height (floor (* (height slider) 0.8)))	 
	 (bob-x (floor (+ (- bar-x (/ bob-width 2))
			  (* bar-width
			     (/ (current-value slider) (max-value slider)))
			  )))
	 (bob-y (+ (y slider) top-padding))
	 

	 (slider-bob (sdl2:make-rect bob-x bob-y bob-width bob-height))
	)
    (sdl2:render-draw-rect *renderer* slider-bar)
    (sdl2:render-fill-rect *renderer* slider-bob))
  )

(defun update-slider-on-mouse-pos (x y slider)
  (setf (current-value slider)
	(floor (* 100 (clamp (/ (- x (x slider))
				(width slider))
			     0
			     1))))
  )


(defun cls ()
  (sdl2:set-render-draw-color *renderer* 0 0 0 255)
  (sdl2:render-clear *renderer*))

(defparameter any-frame (create-frame 0 0 100 20))
(defparameter any-slider (create-slider 300 300 100 20))




(defmethod paint ((f frame))
  (draw-border f)
  )

(defmethod paint ((s slider))
  (draw-slider s)
  )

(defparameter *in-sliderp* nil)

(defun point-outside-rectp (x y rect)
  (or (< x (x rect))
      (> x (+ (x rect) (width rect)))
      (< y (y rect))
      (> y (+ (y rect) (height rect))))
  )

(defun point-inside-rectp (x y rect)
  (not (point-outside-rectp x y rect)))

(defun on-mouse-click (x y state world)
  (declare (ignore state world))
  (when (point-inside-rectp x y any-slider)
    (format t "Clicked")
    (update-slider-on-mouse-pos x y any-slider)
    (setf *in-sliderp* t))
  )

(defun on-mouse-motion (x y xrel yrel state world)
  (declare (ignore xrel yrel state world))
  (when (and *in-sliderp* (= state 0))
    (setf *in-sliderp* nil))
  (when *in-sliderp*
    (update-slider-on-mouse-pos x y any-slider)
    (format t "~a,~a~%" xrel yrel)
    ))

(defun setup ())

(defun update (world))

(defun draw (world)
  (declare (ignore world))
  (cls)
  (paint any-slider)
  )



(toggle-fps)
(defvar *scenario* (make-scenario #'setup #'update #'draw))
(setf (on-draw *scenario*) #'draw)
(start *scenario*)
