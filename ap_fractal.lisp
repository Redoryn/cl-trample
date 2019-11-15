(ql:quickload :cl-trample)
(ql:quickload :computable-reals)
(in-package #:cl-trample)

(setq *read-default-float-format* 'double-float)
;;(setq cr:*creal-tolerance* 10)

(defparameter *fractal-texture* nil)
(defparameter *fractal-x* (cr:*r (/ -5 2)))
(defparameter *fractal-y* (cr:*r 1))
(defparameter *fractal-width* (cr:*r (/ 7 2)))
(defparameter *fractal-height* (cr:*r 2))
(defparameter *do-refresh* t)
(defparameter *max-depth* 5)
(defparameter *zoom-level* 1)

(defun setup ()
  (setf *fractal-texture* (sdl2:create-texture *renderer* sdl2:+pixelformat-rgb888+ 1 800 600))
  (setf *fractal-x* (cr:*r (/ -5 2)))
  (setf *fractal-y* (cr:*r 1))
  (setf *fractal-width* (cr:*r (/ 7 2)))
  (setf *fractal-height* (cr:*r 2)))

(defun x->fractal-x (px)
  (cr:+R *fractal-x* (cr:*R (cr:/R px *width*) *fractal-width*)))

(defun y->fractal-y (py)
  (cr:-R *fractal-y* (cr:*R (cr:*R (cr:/R py *height*)) *fractal-height*)))


(defun escape-time (px py max-depth)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum px py max-depth))
  
  
  (let ((x0 (x->fractal-x px))
	(y0 (y->fractal-y py))
	(x (cr:*R 0))
	(y (cr:*R 0))
	(xtemp (cr:*R 0))
	(iteration 0))
    (declare (type cr:creal x0 y0 x y xtemp))
    (declare (type fixnum iteration))
    (loop
       do
	 (setf xtemp (cr:+R (cr:-R (cr:*R x x)
			   (cr:*R y y))
			x0))
	 (setf y (cr:+R (cr:*R 2 x y) y0))
	 (setf x xtemp)
	 (setf iteration (+ 1 iteration))
       until (or (>= (+ (cr:*R x x) (cr:*R y y)) 4.0)
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
	  ;;(count (* *width* *height*))
	  (count 10000)
	  (max-depth *max-depth*))      
      (declare (type SB-SYS:system-area-pointer tex-ptr))
      (declare (type fixnum count))
            ;;clear texture
      (dotimes (i (* *width* *height*))
	(setf (cffi:mem-aref tex-ptr :unsigned-int i)
	      (sdl2:map-rgb *window-format* 0 0 0)))
      (print (type-of tex-ptr))
      (dotimes (n count)
	(setf i (random (* *width* *height*)))
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


(defun zoom-in (px py)
  (let ((fx (x->fractal-x px))
	  (fy (y->fractal-y py))
	  (f-width (cr:*R (/ 3 10) *fractal-width*))
	  (f-height (cr:*R (/ 3 10) *fractal-height*)))
      (setf *zoom-level* (+ 1 *zoom-level*))
      (format t "At: ~a, ~a ~% Zoom: ~a~%"
              fx fy *max-depth*)
      (recalculate (cr:-R fx (cr:*R f-width (/ 1 2)))
		   (cr:+R fy (cr:*R f-height (/ 1 2)))
		   f-width
		   f-height)
      )
  )


(defun reset-view ()
  (setf *zoom-level* 1)
  (setf *max-depth* 100)
  (recalculate (cr:*R (/ -5 2))
	       (cr:*R 1)
	       (cr:*R (/ 7 2))
	       (cr:*R 2))
  )


(defun on-mouse-click (px py state button world)
  (declare (ignore world))
  (print button)
  (cond
    ((and (eq state 1)
	  (eq button 1)) (zoom-in px py))
    ((eq button 2) (reset-view))
    )
  )

(defun draw (world)
  (declare (ignore world))
  )



(start (make-scenario #'setup #'update #'draw))


