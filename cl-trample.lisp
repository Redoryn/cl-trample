;;;; cl-trample.lisp

(in-package #:cl-trample)


(defparameter *height* 600)
(defparameter *width* 800)
(defparameter *renderer* nil)
(defparameter *window-surface* nil)
(defparameter *window-format* nil)
(defparameter *fps-timer* nil)
(defparameter *counted-frames* 0)
(defparameter *bg-clear* nil)

(defun draw-entities (entities)
  (sdl2:render-clear *renderer*)
  (sdl2:set-render-draw-color *renderer* 0 0 0 0)  
  (dolist (entity entities)
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



(defun fps ()
  (let ((avgFps (/ *counted-frames* (/ (sdl2:get-ticks) 1000.0))))
    (print avgFps)
    ))

(defun on-mouse-click (x y xrel yrel state))

(defun start (setup update draw)
  (initialize setup update draw))


(defun initialize (setup-fn update-fn draw-fn)
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Let's make it happen!" :w *width* :h *height* :flags '(:shown))
      (setf *window-surface* (sdl2:get-window-surface win))
      (setf *window-format* (sdl2:surface-format *window-surface*))
      (setf *bg-clear* (sdl2:map-rgb *window-format* 0 0 0))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
	(setf *renderer* renderer)
	(let ((world (funcall setup-fn)))
	  (sdl2:with-event-loop (:method :poll)
	    (:keyup
	     (:keysym keysym)
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	       (sdl2:push-event :quit)))
	    (:mousebuttondown (:x x :y y :state state) (on-mouse-click x y state))
	    (:idle
	     ()
	     (funcall update-fn world)
	     (funcall draw-fn world)
	     (sdl2:render-present *renderer*)
	     (setf *counted-frames* (1+ *counted-frames*))
	     ;(fps)
	     (sdl2:delay 16)
	     )
	    (:quit () t)))

	))))
