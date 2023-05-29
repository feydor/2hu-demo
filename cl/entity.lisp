;;;; entity.lisp
;;;; Provides an entity base class and methods to change its position/draw it/etc

(in-package :cl-user)
(defpackage #:2hu.entity
  (:use :cl)
  (:import-from :trivial-gamekit
                :draw-image
                :x :y
                :vec2)
  (:export #:entity
           #:entity-draw
           #:entity-update-pos))
(in-package :2hu.entity)

(defconstant +marisa-anim-frames+ 8)

(defclass entity () 
  ((pos :accessor entity-pos 
        :initarg :pos
        :initform (vec2 0.0 0.0)
        :documentation "Entity position vector.")
   (rect :accessor entity-rect
         :initarg :rect
         :initform (vec2 64 100)
         :documentation "Entity bounding rectangle.")
   (dead :accessor entity-dead
         :initarg :dead
         :initform nil
         :type boolean
         :documentation "Is dead boolean.")
   (move-speed :accessor entity-move-speed
               :initarg :move-speed
               :type number
               :initform 9
               :documentation "Pixels per frame movement speed.")
   (anim-cur :accessor entity-anim-cur
             :initarg :anim-cur
             :initform nil
             :documentation "The current animation to play.")
   (anim-index :accessor entity-anim-index 
               :initarg :anim-index
               :type integer
               :initform 0
               :documentation "The index in the current animation to play.")
   (hp :accessor entity-hp
       :initarg :hp
       :initform 1
       :documentation "Entity's health points")))

(defmethod entity-draw ((e entity))
  (progn
    (draw-anim-cur e)
    (inc-anim e)))

(defmethod entity-update-pos ((e entity) keys-pressed bounding-w bounding-h)
  "Updates the position of the entity depending on 
   keys-pressed list. Does bounds checking."
  (when (or (member :a keys-pressed)
            (member :left keys-pressed))
    (decf (x (entity-pos e)) (entity-move-speed e)))
  (when (or (member :d keys-pressed)
            (member :right keys-pressed))
    (incf (x (entity-pos e)) (entity-move-speed e)))
  (when (or (member :w keys-pressed)
            (member :up keys-pressed))
    (incf (y (entity-pos e)) (entity-move-speed e)))
  (when (or (member :s keys-pressed)
            (member :down keys-pressed))
    (decf (y (entity-pos e)) (entity-move-speed e)))
  (contain-entity e bounding-w bounding-h))

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
                                    (* 2 (entity-height e)))
                      :width (entity-width e)
                      :height (entity-height e)))

(defmethod inc-anim ((e entity))
  (if (= (entity-anim-index e) +marisa-anim-frames+)
      (setf (entity-anim-index e) 0)
      (incf (entity-anim-index e))))

(defmethod entity-width ((e entity))
  (x (entity-rect e)))

(defmethod entity-height ((e entity))
  (y (entity-rect e)))
