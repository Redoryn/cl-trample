;;;; cl-trample.lisp

(in-package #:cl-trample)

;;(setf red-color (sdl2:map-rgb *window-format* 255 0 0))
;;(setf green-color (sdl2:map-rgb *window-format* 0 255 0))


(defun update ()
  (dolist (e *entities*)
    (setf (width e) (either (+ 1 (width e))
			    (+ -1 (width e))
			    ))
    (setf (height e) (either (+ 1 (height e))
			     (+ -1 (height e))
			    ))d
    (setf (angle e) (+ (random 6.34) (angle e)))
    (setf (pos-x e) (wrap-value (floor (+ (pos-x e) (vel-x e))) 0 *width*))
    (setf (pos-y e) (wrap-value (floor (+ (pos-y e) (vel-y e))) 0 *height*))
    ))



(defun initialize-entities ()
  (dotimes (n 1000)
    (let ((e (make-instance 'entity
  			    :x (random *width*)
  			    :y (random *height*)
  			    :vel-x (* 3 (- (random 2.0) 1))
  			    :vel-y (* 3 (- (random 2.0) 1))
  			    :w 10
  			    :h 10)))
      (setf (sprite e) (either
			(get-sprite "red")
			(get-sprite "ball")
			))
      (setf (dest-rect (sprite e)) (sdl2:make-rect 0 0 0 0) )
      (setf *entities* (cons e *entities*))
      ))
  )

(defun render ()
  (sdl2:render-clear *renderer*)
  (sdl2:set-render-draw-color *renderer* 0 0 0 0)  
  (dolist (entity *entities*)
    (let* ((x (pos-x entity))
	   (y (pos-y entity))
	   (texture (texture (sprite entity)))
	   (src-rect (src-rect (sprite entity)))
	   (dest-rect (dest-rect (sprite entity))))
      (setf (sdl2:rect-x dest-rect) x)
      (setf (sdl2:rect-y dest-rect) y)
      (setf (sdl2:rect-width dest-rect) (width entity))
      (setf (sdl2:rect-height dest-rect) (height entity))
      
      (sdl2:render-copy-ex *renderer* texture :source-rect src-rect :dest-rect dest-rect :angle (angle entity))
      )))




(defparameter *height* 1000)
(defparameter *width* 1800)
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
