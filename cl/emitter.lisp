;;;; emitter.lisp
;;;; Emitters spawn enemies

(in-package :cl-user)
(defpackage :2hu.emitter
  (:use :cl :common-functions)
  (:import-from :2hu.entity
                :make-entity
                :make-shot
                :entity-dead-p
                :entity-x
                :entity-y
                :entity-print)
  (:import-from :trivial-gamekit
                :vec2
                :x :y)
  (:import-from :2hu.entity.enemy
                :make-enemy)
  (:export #:make-emitter
           #:emitter-update))

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
             :initform 9
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
                 :aim-func #'angle-with-entity
                 :spawn-entity-func #'make-enemy
                 :spawn-entity-p #'periodic))

;; TODO: Move anim, anim-frames, rect into a seperate object called animation
;;       Pass it into the constructor make-emitter
(defmethod emitter-update ((e emitter) entities target-entity time anim anim-frames bounds-w bounds-h)
  "For each tick, the emitter emits entites into the entities list based on its functions."
  (when (periodic time)
    (let* ((angle (funcall (aim-func e) e target-entity))
          (dx (velocity-x (emitter-velocity e) angle))
          (dy (velocity-y (emitter-velocity e) angle)))
      (vector-push-extend (make-enemy (vec2 400 790)
                                      (vec2 0 -5)
                                      :anim anim
                                      :anim-frames anim-frames
                                      :rect (vec2 30 26))
                          entities)
     )))
      
(defmethod angle-with-entity ((e emitter) entity)
  (angle-between-two-points (vec2 (emitter-x e) (emitter-y e))
                            (vec2 (entity-x entity) (entity-y entity))))

(defun periodic (time)
  (= (mod time 60) 0))

(defun spawn-constantly () t)

(defun angle-between-two-points (first second)
  (cos (/ (- (x first) (x second))
          (- (y first) (y second)))))

(defun velocity-x (velocity radians)
  "Returns the x-component of the velocity given an angle in radians."
  (* velocity (cos radians)))

(defun velocity-y (velocity radians)
  "Returns the y-component of the velocity given an angle in radians."
  (* velocity (sin radians)))
