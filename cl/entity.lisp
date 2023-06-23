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
  ; Not sure if this is actually removing from the global *entities* list...
  (let ((still-alive (list-2-vector (remove-if #'entity-dead-p entities))))
    (cond ((eq 0 (length still-alive)) t)
          ((eq (type-of (elt still-alive 0)) 'enemy)
              (setq *enemies* still-alive))
          (t (setq *shots* still-alive)))))

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

;if (x1+w1<x2 || y1+h1<y2 || x1 > x2+w2 || y1 > y2+h2) {
;    return false;
;}
;; Calculate the opposite of overlap, then invert the result
(defmethod entity-overlap-p ((e entity) other)
  (let ((x1 (x (entity-position e)))
        (y1 (y (entity-position e)))
        (w1 (entity-width e))
        (h1 (entity-height e))
        (x2 (x (entity-position other)))
        (y2 (y (entity-position other)))
        (w2 (entity-width other))
        (h2 (entity-height e)))
    (not (or (< (+ x1 w1) x2)
             (< (+ y1 h1) y2)
             (> x1 (+ x2 w2))
             (> y1 (+ y2 h2))))))

(defmethod entity-print ((e entity))
  (let ((out t))
    (progn
      (print-unreadable-object (e t :type t)
        (log:info t "~s" (entity-position e)))
      e)))

;;;; player.lisp

(defclass player (entity) () (:documentation "A player entity."))

(defun make-player (initial-position velocity-x velocity-y height width anim anim-frames)
  (make-instance 'player
                 :x (x initial-position)
                 :y (y initial-position)
                 :velocity-x velocity-x
                 :velocity-y velocity-y
                 :anim-cur anim
                 :anim-frames anim-frames
                 :rect (vec2 width height)
                 :hp 3))

(defmethod entity-update ((p player) keys-pressed bounding-w bounding-h)
  "Updates the position of the player depending on
   keys-pressed list."
  (handle-player-movement p keys-pressed)
  (contain-entity p bounding-w bounding-h))

(defmethod handle-player-movement ((p player) keys-pressed)
  "Change the players position based on the keys-pressed."
  (when (or (member :a keys-pressed)
            (member :left keys-pressed))
    (decf (entity-x p) (entity-velocity-x p)))
  (when (or (member :d keys-pressed)
            (member :right keys-pressed))
    (incf (entity-x p) (entity-velocity-x p)))
  (when (or (member :w keys-pressed)
            (member :up keys-pressed))
    (incf (entity-y p) (entity-velocity-y p)))
  (when (or (member :s keys-pressed)
            (member :down keys-pressed))
    (decf (entity-y p) (entity-velocity-y p))))

;;; enemy.lisp

(defclass enemy (entity) () (:documentation "An enemy entity."))

(defun make-enemy (position velocity &key anim anim-frames rect)
  "Make an enemy at (x, y) with an initial velocity."
  (make-instance 'enemy
                 :x (x position)
                 :y (y position)
                 :anim-cur anim
                 :anim-frames anim-frames
                 :hp 1
                 :rect rect
                 :velocity-x (x velocity)
                 :velocity-y (y velocity)))

(defmethod entity-update ((e enemy) unused bounding-w bounding-h)
  "Update an enemy."
  (progn
    (incf (entity-y e) (entity-velocity-y e))
    (incf (entity-x e) (entity-velocity-x e))
    (when (entity-out-of-bounds-p e bounding-w bounding-h)
      (setf (entity-dead-p e) t))))
