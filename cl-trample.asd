;;;; cl-trample.asd

(asdf:defsystem #:cl-trample
  :description "Describe cl-trample here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl2 #:cffi #:alexandria #:sdl2-ttf)
  :components ((:file "package")
	       (:file "common")
               (:file "cl-trample")
	       (:file "entity")
	       (:file "texture-atlas")))
