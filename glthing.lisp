;;;; glthing.lisp

(in-package #:glthing)

(defconstant +pif+ (coerce pi 'single-float))
(defparameter *angle* 0)
(defvar *gl-thread*)
(defvar *dev-pi* (/ +pif+ 180))

(defun theta ()
  (if (>= *angle* 360)
      (setf *angle* 0)
      (incf *angle* 0.02)))

(defun new-draw-circle (cx cy radius num-segments)
  (declare (single-float cx)
           (single-float cy)
           (single-float radius)
           (integer num-segments))
  (let* ((new-theta (/ (* 2 3.1415926) num-segments))
         (tangetial-factor (tan new-theta))
         (radial-factor (cos new-theta))
         (x radius)
         (y 0))
    (gl:begin :LINE-LOOP)
    (loop for ii below num-segments
       do (progn
            (gl:vertex (+ x cx) (+ y cy))
            (let ((tx (- y))
                  (ty x))
              (setq x (+ x (* tx tangetial-factor))
                    y (+ y (* ty tangetial-factor)))
              (setq x (* x radial-factor)
                    y (* y radial-factor)))))
    (gl:end)))

(defun draw-circle (radius)
  (declare (single-float radius))
  (gl:with-primitive :line-strip
    (do* ((i 0 (1+ i))
          (the-angle 0 (* *dev-pi* i)))
         ((> i 360))
      (gl:vertex (* (cos the-angle) radius)
                 (* (sin the-angle) radius)))
    #++(loop for i below 360
          for the-angle = (* i *dev-pi*)
          do (gl:vertex (* (cos the-angle) radius)
                        (* (sin the-angle) radius)))))

(defclass cube-window (glut:window)
  ()
  (:default-initargs :width 500 :height 500 :title "add - glthing"
                     :mode '(:double :rgb)))

(defmethod glut:display-window :before ((w cube-window))
  (gl:clear-color 0 0 0 0)
  (gl:shade-model :flat))

(defmethod glut:display ((w cube-window))
  (gl:clear :color-buffer :depth-buffer)
  (gl:enable :blend :line-smooth)
  (gl:line-width 1.5)
  (gl:blend-func :src-alpha :one)
  (dotimes (i 255)
    #++(gl:color (random 1.0) (random 1.0) (random 1.0) (/ i 200.0))
    (gl:color (/ i 32.0) (/ i 512.0) (/ i 512.0) (/ i 255.0))
    (gl:load-identity)                       ;; clear the matrix
    (glu:look-at (* i 0.15) 3 5 0 0 0 0 1 0) ;; viewing transformation
    (gl:rotate (theta) -1.0 1.0 1.0)
    #++(gl:with-pushed-matrix* (:modelview)
         (gl:scale 1 2 3)                       ;; modeling transformation
         (glut:wire-cube 2.0))
    (gl:rotate (* i 2) 1.0 -1.0 1.0)
    ;;(draw-circle 5.0)
    (new-draw-circle 5.0 5.0 5.0 20)
    (gl:rotate (* i 1) 1.0 -1.0 1.0)
    ;;(draw-circle 5.0)
    (new-draw-circle 5.0 5.0 5.0 20)
    (gl:color (/ i 512.0) (/ i 512.0) (/ i 64.0) (/ i 255.0))
    (gl:scale 0.8 0.8 0.8)
    (gl:rotate (* i 1) 1.0 1.0 -1.0)
    ;;(draw-circle 5.0)
    (new-draw-circle 5.0 5.0 5.0 20)
    )
  #++(opengl-test 70.0)
  (glut:swap-buffers))

(defmethod glut:idle ((w cube-window))
  (glut:post-redisplay)
  (sleep 0.02))

(defmethod glut:reshape ((w cube-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:frustum -1 1 -1 1 1.5 200)
  (gl:matrix-mode :modelview))

(defmethod glut:keyboard ((w cube-window) key x y)
  (declare (ignore x y))
  (when (eql key #\Esc)
    (glut:destroy-current-window)))

(defun rb-cube ()
  (glut:display-window (make-instance 'cube-window)))


#++(defun my-gl-loop (degree)
  (let ((size 0.01))
    (gl:load-identity)
    (gl:translate 0.0 -1.0 0.0)
    (gl:scale 1.0 1.0 1.0)
     (dotimes (i 100)
      (gl:translate (/ i 200.0) 0.0 0.0)
      (gl:color (/ i 150.0) 0.0 1.0)
      (gl:rotate degree (/ i 20000.0) 0.5 0.0)
      (gl:with-primitive :quads
        (gl:vertex 0.0 0.0)
        (gl:vertex size 0.0)
        (gl:vertex size size)
        (gl:vertex 0.0 size)))))

#++(defun opengl-test (degree)
  (do ((j degree (+ j 0.005)))
      (nil)
    (my-gl-loop degree)
    (sleep 0.05)))


#++(defun run-test (&optional (a 70.0))
  (bt:make-thread
   (lambda ()
     (opengl-test a))))



(defun run-stuff ()
  (setf *gl-thread* (bt:make-thread
                     (lambda ()
                       (rb-cube)) :name "GL-main" )))
