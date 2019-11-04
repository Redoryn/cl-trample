(ql:quickload :cl-trample)
(in-package #:cl-trample)

(defun initialize-entities ()
  (let ((entities '()))
    (dotimes (n 1000)
      (let ((e (create-entity (random *width*)
			      (random *height*)
			      (either
			       (get-sprite "red")
			       (get-sprite "ball")))
	      ))
	(setf (vel-x e) (* 3 (- (random 2.0) 1)))
	(setf (vel-y e) (* 3 (- (random 2.0) 1)))
	(setf entities (cons e entities))
	))
    entities))

(defun update (entities)
  (dolist (e entities)
    (setf (width e) (either (+ 1 (width e))
			    (+ -1 (width e))
			    ))
    (setf (height e) (either (+ 1 (height e))
			     (+ -1 (height e))
			    ))
    (setf (angle e) (+ (random 6.34) (angle e)))
    (setf (pos-x e) (wrap-value (floor (+ (pos-x e) (vel-x e))) 0 *width*))
    (setf (pos-y e) (wrap-value (floor (+ (pos-y e) (vel-y e))) 0 *height*))
    ))

(defun draw (entities)
  (draw-entities entities))


(start (lambda ()
	 (texture-atlas)
	 (initialize-entities)
	 )
       'update
       'draw
       )
