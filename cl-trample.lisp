;;;; cl-trample.lisp

(in-package #:cl-trample)

;;(setf red-color (sdl2:map-rgb *window-format* 255 0 0))
;;(setf green-color (sdl2:map-rgb *window-format* 0 255 0))

(defun either (option1 option2)
  (if (= 0 (random 2))
      (funcall option1)
      (funcall option2)))


(defun update ()
  (dolist (e *entities*)
    (setf (width e) (either (lambda () (+ 1 (width e)))
			    (lambda () (+ -1 (width e)))
			    ))
    (setf (height e) (either (lambda () (+ 1 (height e)))
			    (lambda () (+ -1 (height e)))
			    ))
    (setf (pos-x e) (floor (+ (pos-x e) (vel-x e))))
    (setf (pos-y e) (floor (+ (pos-y e) (vel-y e))))
    (cond
      ((< (pos-x e) 0) (setf (pos-x e) (+ (pos-x e) *width*)))
      ((>= (pos-x e) *width*) (setf (pos-x e) (- (pos-x e) *width*))))
    (cond
      ((< (pos-y e) 0) (setf (pos-y e) (+ (pos-y e) *height*)))
      ((>= (pos-y e) *height*) (setf (pos-y e) (- (pos-y e) *height*))))
    ))



(defun initialize-entities ()
  (dotimes (n 500)
    (let ((e (make-instance 'entity
  			    :x (random *width*)
  			    :y (random *height*)
  			    :vel-x (* 3 (- (random 2.0) 1))
  			    :vel-y (* 3 (- (random 2.0) 1))
  			    :w 10
  			    :h 10)))
      (setf (texture e) (get-texture "foo"))
      (setf *entities* (cons e *entities*))
      ))
  )

(defun coord->index (x y width)
  (declare (type integer x y width))
  (+ x (* y width)))

(defun index->coord (i width)
  (values (mod i width) (floor i width)))

(defun wrap-pos (x y width height)
  (list (cond
	    ((< x 0) (+ width x))
	    ((>= x width) (- x width))
	    (t x))
	  (cond
	    ((< y 0) (+ height y))
	    ((>= y height) (- y height))
	    (t y))))

(defun render ()
  (sdl2:render-clear *renderer*)
  (sdl2:set-render-draw-color *renderer* 0 0 0 0)  
  (dolist (entity *entities*)
    (let* ((x (pos-x entity))
	   (y (pos-y entity))
	   (src-rect (texture entity))
	   (dest-rect (sdl2:make-rect x y (width entity) (height entity))))
      (sdl2:render-copy *renderer* *texture-atlas-texture* :source-rect src-rect :dest-rect dest-rect)
      )))




(defparameter *height* 600)
(defparameter *width* 800)
(defparameter *renderer* nil)
(defparameter *window-surface* nil)
(defparameter *window-format* nil)
(defparameter *fps-timer* nil)
(defparameter *counted-frames* 0)
(defparameter *bg-clear* nil)
(defparameter *entities* '())

(defun fps ()
  (let ((avgFps (/ *counted-frames* (/ (sdl2:get-ticks) 1000.0))))
    (print avgFps)
  ))

(defun start ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Let's make it happen!" :w *width* :h *height* :flags '(:shown))
      (setf *window-surface* (sdl2:get-window-surface win))
      (setf *window-format* (sdl2:surface-format *window-surface*))
      (setf *bg-clear* (sdl2:map-rgb *window-format* 0 0 0))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
	(setf *renderer* renderer)
        (setf *texture-atlas-texture* (sdl2:create-texture *renderer* sdl2:+pixelformat-rgb888+ 2 *width* *height*))
	
	(texture-atlas)	
	(initialize-entities)

	(sdl2:with-event-loop (:method :poll)
	  (:keyup
	   (:keysym keysym)
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	     (sdl2:push-event :quit)))
	  (:idle
	   ()
	   (update)
	   (render)
	   (sdl2:render-present *renderer*)
	   (setf *counted-frames* (1+ *counted-frames*))
	   (fps)
	   (sdl2:delay 16)
	   )
	  (:quit () t))

	))))
