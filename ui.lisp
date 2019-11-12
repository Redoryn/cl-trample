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

(defclass slider (frame)
  ((min-value :initform 0 :initarg :min-value :accessor min-value)
   (max-value :initform 100 :initarg :max-value :accessor max-value)
   (current-value :initform 100 :initarg :current-value :accessor current-value)
   (step-size :initform 0 :initarg :step :accessor step-size)))

(defclass label (frame)
  ((text :initform "" :initarg :text :accessor text)))

(defclass button (frame)
  ((text :initform "" :initarg :text :accessor text)
   (pressedp :initform nil :accessor pressedp)))


(defun create-frame (x y width height)
  (make-instance 'frame :x x :y y :width width :height height))

(defun create-slider (x y width height)
  (let ((s (make-instance 'slider :x x :y y :width width :height height)))
    s
    )
  )

(defun initialize-element (elem x y text)
  (with-slots ((label-x x) (label-y y) (label-text text)) elem
    (setf label-x x
	  label-y y
	  label-text text
	  ))
  elem)

(defun create-label (x y text)
  (initialize-element (make-instance 'label) x y text))

(defun create-button (x y text)
  (initialize-element (make-instance 'button) x y text))

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

(defun draw-label (label)
  (let ((side-padding 4)
	(top-padding 4))
    (multiple-value-bind (width height) (draw-text (+ side-padding (x label)) (+ top-padding (y label)) (text label))
      (setf (width label) (+ side-padding side-padding width))
      (setf (height label) (+ top-padding top-padding height))
      (draw-border label)
      )))

(defun draw-button (button)
  (let ((outer-side-padding 4)
	(outer-top-padding 4)
	(side-padding 4)
	(top-padding 4))
    (multiple-value-bind (width height) (draw-text (+ outer-side-padding side-padding (x button))
						   (+ outer-top-padding top-padding (y button)) (text button))
      (let* ((inner-rect (sdl2:make-rect (+ (x button) outer-side-padding)
					 (+ (y button) outer-top-padding)
					(+ side-padding side-padding width)
					(+ top-padding top-padding height)
					))
	     (outer-rect (sdl2:make-rect (x button)
					 (y button)
					 (+ width outer-side-padding outer-side-padding side-padding side-padding)
					 (+ height outer-top-padding outer-top-padding top-padding top-padding))))
	(sdl2:set-render-draw-color *renderer* 0 50 0 255)
	(sdl2:render-fill-rect *renderer* inner-rect)
	(draw-text (+ outer-side-padding side-padding (x button))
		   (+ outer-top-padding top-padding (y button)) (text button))
	(sdl2:render-draw-rect *renderer* outer-rect)
	)
      ))
  )

(defun update-slider-on-mouse-pos (x y slider)
  (setf (current-value slider)
	(floor (* 100 (clamp (/ (- x (x slider))
				(width slider))
			     0
			     1))))
  )

(defun draw-text (x y text)
  (let* ((surface (sdl2-ttf:render-text-solid *font*
					    text
					    0
					    255
					    0
					    0))
	 (texture (sdl2:create-texture-from-surface *renderer* surface))
	 (dest-rect (sdl2:make-rect x y (sdl2:texture-width texture) (sdl2:texture-height texture))))
    (sdl2:free-surface surface)
    (sdl2:render-copy *renderer* texture :dest-rect dest-rect)
    (values (sdl2:texture-width texture) (sdl2:texture-height texture)))
  )


(defun cls ()
  (sdl2:set-render-draw-color *renderer* 0 0 0 255)
  (sdl2:render-clear *renderer*))

(defparameter any-frame (create-frame 0 0 100 20))
(defparameter any-slider (create-slider 300 300 100 20))
(defparameter any-label (create-label 100 100 "Hello World"))
(defparameter any-button (create-button 200 200 "Click me!"))




(defmethod paint ((f frame))
  (draw-border f)
  )

(defmethod paint ((s slider))
  (draw-slider s)
  )

(defmethod paint ((l label))
  (draw-label l)
  )

(defmethod paint ((b button))
  (draw-button b)
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
    (setf (text any-label) (format nil "Value: ~a" (current-value any-slider)))
    ))

(defun setup ())

(defun update (world))

(defun draw (world)
  (declare (ignore world))
  (cls)
  (paint any-slider)
  (paint any-label)
  (paint any-button)
  )



(toggle-fps)
(defvar *scenario* (make-scenario #'setup #'update #'draw))
(setf (on-draw *scenario*) #'draw)
(start *scenario*)
