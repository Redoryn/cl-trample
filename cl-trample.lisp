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
  (let* ((avgFps (round (/ *counted-frames* (/ (sdl2:get-ticks) 1000.0))))
	 (surface (sdl2-ttf:render-text-solid *font*
					      (format nil "FPS: ~d " avgFps)
					      0
					      255
					      0
					      0))
	 (texture (sdl2:create-texture-from-surface *renderer* surface))
	 (dest-rect (sdl2:make-rect 10 10 (sdl2:texture-width texture) (sdl2:texture-height texture)))
	 (sdl2:free-surface surface)
	 )
    (sdl2:render-copy *renderer* texture :dest-rect dest-rect)))

(defun on-mouse-click (x y state world))
(defun on-mouse-motion (x y xrel yrel state world))

(defclass scenario ()
  ((on-setup :initarg :on-setup :initform nil :accessor on-setup)
   (on-update :initarg :on-update :initform nil :accessor on-update)
   (on-draw :initarg :on-draw :initform nil :accessor on-draw)))

(defmethod start ((s scenario))
  (initialize s))

(defun make-scenario (setup update draw)
  (let ((scenario (make-instance 'scenario)))
    (setf (on-setup scenario) setup)
    (setf (on-update scenario) update)
    (setf (on-draw scenario) draw)
    scenario))

(defparameter *show-fps* nil)
(defparameter *font* nil)

(defmethod toggle-fps ()
  (setf *show-fps* (not *show-fps*)))

(defun initialize (scenario)
  (sdl2-ttf:init)
  (setf *font* (sdl2-ttf:open-font "C:/users/Alex/Google Drive/Development/clisp/PROBE_10PX_OTF.otf" 10))
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Let's make it happen!" :w *width* :h *height* :flags '(:shown))
      (setf *window-surface* (sdl2:get-window-surface win))
      (setf *window-format* (sdl2:surface-format *window-surface*))
      (setf *bg-clear* (sdl2:map-rgb *window-format* 0 0 0))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
	(setf *renderer* renderer)
	(let ((world (funcall (on-setup scenario))))
	  (sdl2:with-event-loop (:method :poll)
	    (:keyup
	     (:keysym keysym)
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	       (sdl2:push-event :quit)))
	    (:mousebuttondown (:x x :y y :state state)
			      (on-mouse-click x y state world))
	    (:mousebuttonup (:x x :y y :state state)
			    (on-mouse-click x y state world))
	    (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
			  (on-mouse-motion x y xrel yrel state world))
	    (:idle
	     ()
	     (funcall (on-update scenario) world)
	     (funcall (on-draw scenario) world)
	     
	     (setf *counted-frames* (1+ *counted-frames*))
	     (when *show-fps*
	       (fps))
	     (sdl2:render-present *renderer*)
	     (sdl2:delay 16)
	     )
	    (:quit () t)))

	))))
