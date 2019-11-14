(ql:quickload :cl-trample)
(in-package :cl-trample)

(defclass ui-element ()
  ((x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)
   (height :initform 0 :initarg :height :accessor height)
   (width :initform 0 :initarg :width :accessor width)
   (parent :initform nil :initarg :parent :accessor parent))
  )

(defclass frame (ui-element)
  ((children :initform nil :accessor children)))

(defclass slider (ui-element)
  ((min-value :initform 0 :initarg :min-value :accessor min-value)
   (max-value :initform 100 :initarg :max-value :accessor max-value)
   (current-value :initform 100 :initarg :current-value :accessor current-value)
   (step-size :initform 0 :initarg :step :accessor step-size)))

(defclass label (ui-element)
  ((text :initform "" :initarg :text :accessor text)))

(defclass button (ui-element)
  ((text :initform "" :initarg :text :accessor text)
   (pressedp :initform t :accessor pressedp)))

(defmethod absolute-pos ((u ui-element))
  (if (null (parent u))
      (values (x u) (y u))
      (multiple-value-bind (px py) (absolute-pos (parent u))
	(values (+ px (x u))
		(+ py (y u))))
      )
  )



(defmethod add-child ((f frame) child)
  (with-slots (children) f
    (when (null children)
      (setf children '()))
    (setf children (cons child children))
    (setf (parent child) f)
    )
  (values)
  )

(defun initialize-element (elem x y text)
  (with-slots ((elem-x x) (elem-y y) (elem-text text)) elem
    (setf elem-x x
	  elem-y y
	  elem-text text
	  ))
  elem)

(defun create-frame (x y width height)
  (let ((frame (make-instance 'frame :x x :y y :width width :height height)))
    frame
    ))

(defun create-slider (x y width height)
  (let ((s (make-instance 'slider :x x :y y :width width :height height)))
    s
    )
  )

(defun create-label (x y text)
  (initialize-element (make-instance 'label) x y text))

(defun create-button (x y text)
  (initialize-element (make-instance 'button) x y text))



(defmethod make-rect ((u ui-element))
  (sdl2:make-rect (+ (x u)
		     (if (parent u) (x (parent u)) 0))
		  (+ (y u)
		     (if (parent u) (y (parent u)) 0))
		  (width u)
		  (height u)))

(defun draw-border (ui-element)
  (let ((rect (make-rect ui-element)))
    (sdl2:set-render-draw-color *renderer* 0 255 0 255)
    (sdl2:render-draw-rect *renderer* rect))
  )

(defun draw-slider (slider)
  (draw-border slider)
  (multiple-value-bind (x y) (absolute-pos slider) 
    (let* ((top-padding 2)
	   (side-padding 7)
	   (bar-x (+ side-padding x))
	   (bar-y (floor (+ y (/ (height slider) 2))))
	   (bar-width (- (width slider) (* side-padding 2)))
	   (bar-height 2)
	   (slider-bar (sdl2:make-rect bar-x bar-y bar-width bar-height))
	   
	   (bob-width (floor (* 0.1 bar-width)))
	   (bob-height (floor (* (height slider) 0.8)))	 
	   (bob-x (floor (+ (- bar-x (/ bob-width 2))
			    (* bar-width
			       (/ (current-value slider) (max-value slider)))
			    )))
	   (bob-y (+ y top-padding))
	   

	   (slider-bob (sdl2:make-rect bob-x bob-y bob-width bob-height))
	   )
      (sdl2:render-draw-rect *renderer* slider-bar)
      (sdl2:render-fill-rect *renderer* slider-bob)))
  )

(defun draw-label (label)
  (multiple-value-bind (x y) (absolute-pos label)
    (let ((side-padding 4)
	  (top-padding 4))
      (multiple-value-bind (width height) (draw-text (+ side-padding x) (+ top-padding y) (text label))
	(setf (width label) (+ side-padding side-padding width))
	(setf (height label) (+ top-padding top-padding height))
	(draw-border label)
	))))

(defun draw-button (button)
  (multiple-value-bind (x y) (absolute-pos button)
    (let ((outer-side-padding 4)
	  (outer-top-padding 4)
	  (side-padding 4)
	  (top-padding 4))
      (multiple-value-bind (width height) (draw-text (+ outer-side-padding side-padding x)
						     (+ outer-top-padding top-padding y) (text button))
	(let* ((inner-rect (sdl2:make-rect (+ x outer-side-padding)
					   (+ y outer-top-padding)
					   (+ side-padding side-padding width)
					   (+ top-padding top-padding height)
					   ))
	       (outer-rect (sdl2:make-rect x
					   y
					   (+ width outer-side-padding outer-side-padding side-padding side-padding)
					   (+ height outer-top-padding outer-top-padding top-padding top-padding))))


	  (if (pressedp button)
	      (sdl2:set-render-draw-color *renderer* 0 50 0 255)
	      (sdl2:set-render-draw-color *renderer* 0 100 0 255)
	      )
	  (sdl2:render-fill-rect *renderer* outer-rect)
	  (setf (width button) (sdl2:rect-width outer-rect)
		(height button) (sdl2:rect-height outer-rect))

	  (if (pressedp button)
	      (sdl2:set-render-draw-color *renderer* 0 100 0 255)
	      (sdl2:set-render-draw-color *renderer* 0 50 0 255)
	      )
	  
	  (sdl2:render-fill-rect *renderer* inner-rect)
	  (draw-text (+ outer-side-padding side-padding x)
		     (+ outer-top-padding top-padding y) (text button))
	  ))))
  )

(defun update-slider-on-mouse-pos (x y slider)
  (setf (current-value slider)
	(floor (* 100 (clamp (/ (- x (absolute-pos slider))
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

(defmethod on-click ((u ui-element) event))

(defmethod on-click ((f frame) event)
  (destructuring-bind (x y state) event
    (dolist (child (children f))
      (when (point-inside-element child x y)
	(on-click child event))))  
  )

(defparameter *state-of-button-click* nil)
(defmethod on-click ((b button) event)
  (destructuring-bind (x y state) event
    (setf (text any-label) (format nil "~a" state))
    (when (not (eq *state-of-button-click* state))
      (setf (pressedp b) (not (pressedp b)))
      (setf *state-of-button-click* state)
      ))
  )

(defmethod on-click ((s slider) event)
  (destructuring-bind (x y state) event
    (update-slider-on-mouse-pos x y s)
    (setf *in-sliderp* t))
  )



(defmethod paint ((f frame))
  (draw-border f)
  (dolist (child (children f))
    (paint child))
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


(defparameter any-frame (create-frame 0 0 200 200))
(defparameter any-slider (create-slider 10 10 100 20))
(defparameter any-label (create-label 10 40 "Hello World"))
(defparameter any-button (create-button 10 70 "Click me!"))
(defparameter *root-frame* any-frame)

(add-child any-frame any-slider)
(add-child any-frame any-button)
(add-child any-frame any-label)

(defparameter *in-sliderp* nil)

(defun point-outside-rectp-expanded (px py rx ry rwidth rheight)
  (or (< px rx)
      (> px (+ rx rwidth))
      (< py ry)
      (> py (+ ry rheight)))
  )

(defun point-outside-rectp (x y rect)
  (point-outside-rectp-expanded x y (x rect) (y rect) (width rect) (height rect)))

(defun point-inside-rectp (x y rect)
  (not (point-outside-rectp x y rect)))

(defmethod point-inside-element ((u ui-element) x y)
  (multiple-value-bind (abs-x abs-y) (absolute-pos u)
    (not (point-outside-rectp-expanded x y abs-x abs-y (width u) (height u)))
    )
  )





(defun on-mouse-click (x y state world)
  (declare (ignore state world))
  
  (when (point-inside-element *root-frame* x y)
    (let ((event (list x y state)))
      (on-click *root-frame* event)
      )
    )
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
  (paint any-frame)
  ;;(paint any-slider)
  ;;(paint any-label)
  ;;(paint any-button)
  )



(toggle-fps)
(defvar *scenario* (make-scenario #'setup #'update #'draw))
(setf (on-draw *scenario*) #'draw)
(start *scenario*)
