;;;; entity.lisp
;;;; Provides an entity base class and methods to change its position/draw it/etc

(in-package :cl-user)
(defpackage :2hu.entity
  (:use :cl :common-functions)
  (:import-from :trivial-gamekit
                :draw-image
                :x :y
                :vec2)
  (:export :entity-draw
           :entity-update
           :entity-update-all
           :entity-position
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
  ((x :accessor entity-x
      :initarg :x
      :initform 0
      :documentation "Entity X coordinate.")
   (y :accessor entity-y
      :initarg :y
      :initform 0
      :documentation "Entity Y coordinate.")
   (velocity-x :accessor entity-velocity-x
       :initarg :velocity-x
       :initform 0
       :documentation "Entity Velocity in X.")
   (velocity-y :accessor entity-velocity-y
       :initarg :velocity-y
       :initform 0
       :documentation "Entity Velocity in Y.")
   (rect :accessor entity-rect
         :initarg :rect
         :initform (vec2 64 100)
         :documentation "Entity bounding rectangle.")
   (dead :accessor entity-dead-p
         :initarg :dead
         :initform nil
         :type boolean
         :documentation "Is dead boolean.")
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

(defmethod entity-draw ((e entity))
  (progn
    (draw-anim-cur e)
    (increment-anim e)))

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
  (when (< (entity-x e) 1)
    (setf (entity-x e) 1))
  (when (> (entity-x e) (- w (entity-width e)))
    (setf (entity-x e) (- w (entity-width e))))
  (when (< (entity-y e) 1)
    (setf (entity-y e) 1))
  (when (> (entity-y e) (- h (entity-height e)))
    (setf (entity-y e) (- h (entity-height e)))))

(defmethod draw-anim-cur ((e entity))
  (gamekit:draw-image (entity-position e) (entity-anim-cur e) 
                      :origin (vec2 (* (entity-anim-index e) (entity-width e))
                                    0)
                      :width (entity-width e)
                      :height (entity-height e)))

(defmethod increment-anim ((e entity))
  "Increment the anim-index to anim-frames, looping back to 0 if max is reached"
  (if (= (entity-anim-index e) (entity-anim-frames e))
      (setf (entity-anim-index e) 0)
      (incf (entity-anim-index e))))

(defmethod entity-width ((e entity))
  (x (entity-rect e)))

(defmethod entity-height ((e entity))
  (y (entity-rect e)))

(defmethod entity-position ((e entity))
  (vec2 (entity-x e) (entity-y e)))

(defmethod entity-out-of-bounds-p ((e entity) bounds-w bounds-h)
  (let ((x (x (entity-position e)))
        (y (y (entity-position e))))
    (cond ((< x 1) t)
          ((< y 1) t)
          ((> x (- bounds-w (entity-width e))) t)
          ((> y bounds-h) t)
          (t nil))))

(defmethod entity-print ((e entity))
  (let ((out t))
    (progn
      (print-unreadable-object (e t :type t)
        (log:info t "~s" (entity-position e)))
      e)))
