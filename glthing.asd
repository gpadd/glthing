;;;; glthing.asd

(asdf:defsystem #:glthing
  :depends-on (cl-opengl cl-glu cl-glut glop bordeaux-threads)
  :serial t
  :description "glthing project"
  :author "Andreas Wilfer <unknown.areth@gmail.com>"
  :license "MIT"
  :components ((:file "package")
               (:file "glthing")))
