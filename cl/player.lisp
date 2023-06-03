;;;; player.lisp
;;;; Player subclass of Entity

(in-package :cl-user)
(defpackage :2hu.entity.player
  (:use :cl :2hu.entity)
  (:import-from :trivial-gamekit
                :vec2
                :x
                :y)
  (:export #:make-player))
(in-package :2hu.entity.player)

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
