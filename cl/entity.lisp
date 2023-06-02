;;;; entity.lisp
;;;; Provides an entity base class and methods to change its position/draw it/etc

(in-package :cl-user)
(defpackage :2hu.entity
  (:use :cl :common-functions)
  (:import-from :trivial-gamekit
                :draw-image
                :x :y
                :vec2)
  (:export :entity
           :make-entity
           :entity-draw
           :entity-update
           :entity-update-all
           :entity-pos-clone
           :entity-pos
           :entity-dead-p
           :entity-print
           :entity-x
           :entity-y
           :entity-velocity-x
           :entity-velocity-y
           :entity-out-of-bounds-p
           :contain-entity))
(in-package :2hu.entity)

(defclass entity ()
  ((pos :accessor entity-pos 
        :initarg :pos
        :initform (vec2 0.0 0.0)
        :documentation "Entity position vector.")
   (rect :accessor entity-rect
         :initarg :rect
         :initform (vec2 64 100)
         :documentation "Entity bounding rectangle.")
   (dead :accessor entity-dead-p
         :initarg :dead
         :initform nil
         :type boolean
         :documentation "Is dead boolean.")
   (velocity :accessor entity-velocity
             :initarg :velocity
             :initform (vec2 0 0)
             :documentation "Pixels per frame movement speed.")
   (anim-cur :accessor entity-anim-cur
             :initarg :anim-cur
             :initform nil
             :documentation "The current animation to render.")
   (anim-index :accessor entity-anim-index 
               :initarg :anim-index
               :type integer
               :initform 0
               :documentation "The index in the current animation to play.")
   (anim-frames :accessor entity-anim-frames
                :initarg :anim-frames
                :type number
                :initform 1
                :documentation "The # of frames of animation.")
   (hp :accessor entity-hp
       :initarg :hp
       :initform 1
       :documentation "Entity's health points"))
  (:documentation "An entity base class."))

(defun make-entity (initial-pos &key anim rect (velocity (vec2 0 0)) (anim-frames 9) (hp 1))
  "Construct an entity."
  (make-instance 'entity
                 :pos initial-pos
                 :anim-cur anim
                 :anim-frames anim-frames
                 :hp hp
                 :rect rect
                 :velocity velocity))

(defmethod entity-draw ((e entity))
  (progn
    (draw-anim-cur e)
    (inc-anim e)))

;; overload this in the subclasses
(defgeneric entity-update (entity keys-pressed bounding-w bounding-h)
  (:documentation "Updates the position of the entity depending on 
   keys-pressed list. Does bounds checking."))

(defun entity-update-all (entities keys-pressed bounds-w bounds-h)
  "Call entity-update on each entity in the list. And remove from entities if dead."
  (loop for entity across entities
        do (entity-update entity keys-pressed bounds-w bounds-h))
  ;(setq entities (remove-if-not #'entity-dead-p entities))
  )

(defmethod contain-entity ((e entity) w h)
  "Contain the entity within [0, w) and [0, h)."
  (when (< (x (entity-pos e)) 1)
    (setf (x (entity-pos e)) 1))
  (when (> (x (entity-pos e)) (- w (entity-width e)))
    (setf (x (entity-pos e)) (- w (entity-width e))))
  (when (< (y (entity-pos e)) 1)
    (setf (y (entity-pos e)) 1))
  (when (> (y (entity-pos e)) (- h (entity-height e)))
    (setf (y (entity-pos e)) (- h (entity-height e)))))

(defmethod draw-anim-cur ((e entity))
  (gamekit:draw-image (entity-pos e) (entity-anim-cur e) 
                      :origin (vec2 (* (entity-anim-index e) (entity-width e))
                                    0)
                      :width (entity-width e)
                      :height (entity-height e)))

(defmethod inc-anim ((e entity))
  "Increment the anim-index to anim-frames, looping back to 0 if max is reached"
  (if (= (entity-anim-index e) (entity-anim-frames e))
      (setf (entity-anim-index e) 0)
      (incf (entity-anim-index e))))

(defmethod entity-width ((e entity))
  (x (entity-rect e)))

(defmethod entity-height ((e entity))
  (y (entity-rect e)))

(defmethod entity-velocity-x ((e entity))
  (x (entity-velocity e)))

(defmethod entity-velocity-y ((e entity))
  (y (entity-velocity e)))

(defmethod entity-x ((e entity))
  (x (entity-pos e)))

(defmethod entity-y ((e entity))
  (y (entity-pos e)))

(defmethod entity-pos-clone ((e entity))
  "Returns a clone of the entity-pos."
  (vec2 (x (entity-pos e))
        (y (entity-pos e))))

(defmethod entity-out-of-bounds-p ((e entity) bounds-w bounds-h)
  (let ((x (x (entity-pos e)))
        (y (y (entity-pos e))))
    (cond ((< x 1) t)
          ((< y 1) t)
          ((> x (- bounds-w (entity-width e))) t)
          ((> y bounds-h) t)
          (t nil))))

(defmethod entity-print ((e entity))
  (let ((out t))
    (progn
      (print-unreadable-object (e t :type t)
        (log:info t "~s" (entity-pos e)))
      e)))
