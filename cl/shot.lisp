;;;; shot.lisp
;;; Shot subclass of Entity

(in-package :2hu.entity)

(defclass shot (entity) () (:documentation "A shot entity."))

(defun make-shot (initial-position velocity image rect anim-frames)
  "Makes a shot at initial-position with initial velocity"
  (make-instance 'shot
                 :x (x initial-position)
                 :y (y initial-position)
                 :velocity-x (x velocity)
                 :velocity-y (y velocity)
                 :anim-cur image
                 :rect rect
                 :anim-frames anim-frames
                 :hp 1))

(defmethod entity-update ((s shot) unused bounding-w bounding-h)
  "Update a shot."
  (progn
    (incf (entity-y s) (entity-velocity-y s))
    (incf (entity-x s) (entity-velocity-x s))
    (when (entity-out-of-bounds-p s bounding-w bounding-h)
      (setf (entity-dead-p s) t))))
