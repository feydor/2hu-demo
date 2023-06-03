;;;; emitter.lisp
;;;; Emitters spawn enemies

(in-package :cl-user)
(defpackage :2hu.emitter
  (:use :cl :common-functions)
  (:import-from :2hu.entity
                :make-shot
                :entity-dead-p
                :entity-x
                :entity-y
                :entity-position
                :entity-print)
  (:import-from :trivial-gamekit
                :vec2
                :x :y)
  (:import-from :2hu.entity.enemy
                :make-enemy)
  (:export #:make-emitter
           #:emitter-update
           #:emitter-x
           #:emitter-y))

(in-package :2hu.emitter)

(defclass emitter ()
  ((x :accessor emitter-x
      :initarg :x
      :type number
      :initform 0
      :documentation "Emitter's position on the x axis")
   (y :accessor emitter-y
      :initarg :y
      :type number
      :initform 0
      :documentation "Emitter's position on the y axis")
   (velocity :accessor emitter-velocity
             :initarg velocity
             :type number
             :initform 5
             :documentation "Emission velocity")
   (aim-func :reader aim-func
             :initarg :aim-func
             :initform (spawn-constantly)
             :documentation "Function to aim a spawn (give it dx, dy, and angle).")
   (spawn-entity-func :reader spawn-entity-func
                      :initarg :spawn-entity-func
                      :initform nil
                      :documentation "Function to spawn an entity.")
   (spawn-entity-p :reader spawn-entity-p
                   :initarg :spawn-entity-p
                   :documentation "Function to determine the rate of spawning."))
  (:documentation "Emitter spawns entities at its location with a given speed & angle."))

(defun make-emitter-basic (x y)
  (make-instance 'emitter
                 :x x
                 :y y
                 :aim-func #'slope-with-entity
                 :spawn-entity-func #'make-enemy
                 :spawn-entity-p #'periodic))

;; TODO: Move anim, anim-frames, rect into a seperate object called animation
;;       Pass it into the constructor make-emitter
(defmethod emitter-update ((e emitter) entities target-entity time anim anim-frames bounds-w bounds-h)
  "For each tick, the emitter emits entites into the entities list based on its functions."
  (when (periodic time)
    (let ((gradient (funcall (aim-func e) e target-entity))
          (velocity (emitter-velocity e)))
      (format t "velocity-x: ~a velocity-y: ~a~%"
              (* velocity (x gradient))
              (* velocity (y gradient)))
      (vector-push-extend (make-enemy (emitter-position e)
                                      (vec2 (* velocity (x gradient))
                                            (* velocity (y gradient)))
                                      :anim anim
                                      :anim-frames anim-frames
                                      :rect (vec2 30 26))
                          entities)
     )))
      
(defun slope-with-entity (emitter entity)
  (slope (entity-position entity) (emitter-position emitter)))

(defun periodic (time)
  (= (mod time 60) 0))

(defun spawn-constantly () t)

(defun emitter-position (emitter)
  (vec2 (emitter-x emitter)
        (emitter-y emitter)))

(defun slope (point1 point2)
  "Finds the slope of a line and returns it as a pair."
  (let* ((x1 (x point1))
         (y1 (y point1))
         (x2 (x point2))
         (y2 (y point2))
         (steps (max (abs (- x1 x2))
                     (abs (- y1 y2)))))
    (if (= steps 0)
        (vec2 0 0)
        (vec2 (/ (- x1 x2) steps)
              (/ (- y1 y2) steps)))))
