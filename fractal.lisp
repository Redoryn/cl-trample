(ql:quickload :cl-trample)
(in-package #:cl-trample)

(setq *read-default-float-format* 'double-float)

(defparameter *fractal-texture* nil)
(defparameter *fractal-x* -2.5)
(defparameter *fractal-y* 1)
(defparameter *fractal-width* 3.5)
(defparameter *fractal-height* 2.0)
(defparameter *do-refresh* t)
(defparameter *max-depth* 100)
(defparameter *zoom-level* 1)

(defun setup ()
  (setf *fractal-texture* (sdl2:create-texture *renderer* sdl2:+pixelformat-rgb888+ 1 800 600))
  (setf *fractal-x* -2.5)
  (setf *fractal-y* 1)
  (setf *fractal-width* 3.5)
  (setf *fractal-height* 2.0))

(defun x->fractal-x (px)
  (+ *fractal-x* (* (/ px *width*) *fractal-width*)))

(defun y->fractal-y (py)
  (- *fractal-y* (* (* (/ py *height*)) *fractal-height*)))


(defun escape-time (px py max-depth)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum px py max-depth))
  
  
  (let ((x0 (x->fractal-x px))
	(y0 (y->fractal-y py))
	(x 0.0)
	(y 0.0)
	(xtemp 0.0)
	(iteration 0))
    (declare (type double-float x0 y0 x y xtemp))
    (declare (type fixnum iteration))
    (loop
       do
	 (setf xtemp (+ (- (* x x)
			   (* y y))
			x0))
	 (setf y (+ (* 2 x y) y0))
	 (setf x xtemp)
	 (setf iteration (+ 1 iteration))
       until (or (>= (+ (* x x) (* y y)) 4.0)
		 (>= iteration max-depth))
	 )
    iteration
    )
  )


(defun recalculate (x y width height)
  (setf *fractal-x* x)
  (setf *fractal-y* y)
  (setf *fractal-width* width)
  (setf *fractal-height* height)
  (setf *do-refresh* t)
  (when (= 0 (mod *zoom-level* 2))
    (setf *max-depth* (ceiling (* 1.25 *max-depth*))))
  )


(defun update (world)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum *width* *height*))
  
  (declare (ignore world))
  (when *do-refresh*
    (let ((tex-ptr (sdl2:lock-texture *fractal-texture*))
	  (count (* *width* *height*))
	  (max-depth *max-depth*))
      (declare (type SB-SYS:system-area-pointer tex-ptr))
      (declare (type fixnum count))
      (print (type-of tex-ptr))
      (dotimes (i count)
	(multiple-value-bind (px py) (index->coord i *width*)
	  (setf (cffi:mem-aref tex-ptr :unsigned-int i)
		(let ((depth (escape-time px py max-depth)))
		  (if (>= depth max-depth)
		      (sdl2:map-rgb *window-format* 0 0 0)
		      (sdl2:map-rgb *window-format* (floor (* (/ depth max-depth) 255)) 0 0))))  
	  
	  )
	))
    (sdl2:unlock-texture *fractal-texture*)
    (sdl2:render-copy *renderer* *fractal-texture*)
    (setf *do-refresh* nil)))

(defun on-mouse-click (px py state)
  (declare (ignore state))
  (let ((fx (x->fractal-x px))
	(fy (y->fractal-y py))
	(f-width (* 0.35 *fractal-width*))
	(f-height (* 0.35 *fractal-height*)))
    (setf *zoom-level* (+ 1 *zoom-level*))
    (format t "At: ~a, ~a ~% Zoom: ~a~%"
          fx fy *max-depth*)
    (recalculate (- fx (* f-width 0.5))
		 (+ fy (* f-height 0.5))
		 f-width
		 f-height)
    )
  )

(defun draw (world)
  (declare (ignore world))
  )

(start #'setup #'update #'draw)


