;;;; cl-trample.lisp

(in-package #:cl-trample)

;;(setf red-color (sdl2:map-rgb *window-format* 255 0 0))
;;(setf green-color (sdl2:map-rgb *window-format* 0 255 0))

(defparameter *texture-atlas* nil)
(defun texture-atlas ()
  (list
   (make-array 100
	       :element-type 'integer
	       :initial-element (sdl2:map-rgb *window-format* 255 0 0))))

(defun get-texture (name)
  (car *texture-atlas*))


(defun update ()
  (dolist (e *entities*)
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
  (dotimes (n 1000)
    (let ((e (make-instance 'entity
			    :x (random *width*)
			    :y (random *height*)
			    :vel-x (* 2 (- (random 2.0) 1))
			    :vel-y (* 2 (- (random 2.0) 1))
			    :w 10
			    :h 10)))
      (setf (texture e) (get-texture "foo"))
      (setf *entities* (cons e *entities*))
      )))

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

(defun clear-pixels ()
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array integer) *pixels*))
  
  (let ((count (* *width* *height*)))
    (dotimes (i count)
      (setf (aref *pixels* i) 0))))


(defun update-pixels ()
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (simple-array integer) *pixels*))
  (declare (type integer i))
  (declare (type system-area-pointer tex-ptr))
  

  (let ((tex-ptr (sdl2:lock-texture *texture*))
	(count (* *width* *height*)))
    ;; clear texture
    (dotimes (i count)
      (setf (cffi:mem-aref tex-ptr :unsigned-int i) *bg-clear*))

    ;; copy entity textures onto texture
    (dolist (entity *entities*)
      (let* ((x (pos-x entity))
	     (y (pos-y entity))
	     (texture (texture entity))
	     (width (width entity))
	     (height (height entity))
	     (x-offset 0)
	     (count (* width height)))
	(dotimes (j count)
	  (destructuring-bind (p-x p-y) (wrap-pos (+ x x-offset) y *width* *height*)
	    (setf (cffi:mem-aref tex-ptr :unsigned-int (coord->index p-x p-y *width*))
		  (aref texture j))
	    )
	  (setf x-offset (1+ x-offset))
	  (when (= x-offset width)
	    (setf x-offset 0)
	    (setf y (1+ y)))
	  )
	)))
  (sdl2:unlock-texture *texture*)
  )
		 

(defun render (renderer)
  ;(declare (optimize (speed 3) (safety 0)))
  ;(declare (type integer i))
  ;(declare (type system-area-pointer tex-ptr))
  (update-pixels)
  (sdl2:render-clear renderer)
  
  ;; (let ((tex-ptr (sdl2:lock-texture *texture*))
  ;; 	(count (* *width* *height*)))
  ;;   (dotimes (i count)
  ;;     (setf (cffi:mem-aref tex-ptr :unsigned-int i)
  ;; 	    (aref *pixels* i))
  ;; 	 ))
  ;; (sdl2:unlock-texture *texture*)
  (sdl2:render-copy renderer *texture*))


(defparameter *height* 1000)
(defparameter *width* 1800)
(defparameter *pixels* (make-array (* *height* *width*) :element-type 'integer :initial-element 0))
(defparameter *texture* nil)
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
      (print *window-format*)
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
	(setf *texture* (sdl2:create-texture renderer sdl2:+pixelformat-rgb888+ 1 *width* *height*))
	(setf *texture-atlas* (texture-atlas))
	(initialize-entities)
	(sdl2:with-event-loop (:method :poll)
	  (:keyup
	   (:keysym keysym)
	   (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	     (sdl2:push-event :quit)))
	  (:idle
	   ()
	   (update)
	   (render renderer)
	   (sdl2:render-present renderer)
	   (setf *counted-frames* (1+ *counted-frames*))
	   (fps)
					;(sdl2:delay 16)
	   )
	(:quit () t))))))
