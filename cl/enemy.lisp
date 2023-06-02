;;;; enemy.lisp
;;;; Enemy subclass of Entity

(in-package :cl-user)
(defpackage :2hu.entity.enemy
  (:use :cl :2hu.entity)
  (:import-from :trivial-gamekit
                :vec2
                :x
                :y)
  (:export #:make-enemy))
(in-package :2hu.entity.enemy)

(defclass enemy (entity) () (:documentation "An enemy entity."))

(defun make-enemy (position velocity &key anim anim-frames rect)
  "Make an enemy at (x, y) with an initial velocity."
  (make-instance 'enemy
                 :pos position
                 :anim-cur anim
                 :anim-frames anim-frames
                 :hp 1
                 :rect rect
                 :velocity velocity))

(defmethod entity-update ((e enemy) unused bounding-w bounding-h)
  "Update an enemy."
  (progn
    (incf (y (entity-pos e)) (entity-velocity-y e))
    (when (entity-out-of-bounds-p e bounding-w bounding-h)
      (setf (entity-dead-p e) t))))
