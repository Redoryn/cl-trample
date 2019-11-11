(in-package #:cl-trample)

(defmacro either (option1 option2)
  `(if (= 0 (random 2))
      ,option1
      ,option2))

(defun wrap-value (v min max)
  (cond
    ((< v min) (- max v))
    ((> v max) (- v max))
    (t v))
  )

(defun clamp (v min max)
  (cond
    ((< v min) min)
    ((> v max) max)
    (t v))
  )

(defun coord->index (x y width)
  (declare (type integer x y width))
  (+ x (* y width)))

(defun index->coord (i width)
  (values (mod i width) (floor i width)))




