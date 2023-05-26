;;;; entity.lisp
(in-package :twohu)

(defconstant +marisa-anim-frames+ 8)

(defclass twohu-entity ()
    ((pos  :initarg :pos
           :initform (vec2 0.0 0.0)
           :accessor pos)
     (rect :initarg :rect
           :initform (vec2 64 100)
           :accessor rect)
     (dead :initarg :dead
           :initform nil
           :accessor dead)
     (move-speed :initarg :move-speed
            :initform 9
            :accessor move-speed)
     (anim-cur  :initarg :anim-cur
                :initform nil
                :accessor anim-cur)
     (anim-index :initarg :anim-index
                 :initform 0
                 :accessor anim-index)
     (hp :initarg :hp
         :initform 1
         :accessor hp)))

(defmethod draw-entity ((e twohu-entity))
      (progn
            (draw-anim-cur e)
            (inc-anim e)))

(defmethod update-pos ((e twohu-entity))
      "Updates the position of the entity depending on
            the global *keys-pressed* list. Does bounds checking."
      (when (or (member :a *keys-pressed*)
                (member :left *keys-pressed*))
            (decf (x (pos e)) (move-speed e)))
      (when (or (member :d *keys-pressed*)
                (member :right *keys-pressed*))
            (incf (x (pos e)) (move-speed e)))
      (when (or (member :w *keys-pressed*)
                (member :up *keys-pressed*))
            (incf (y (pos e)) (move-speed e)))
      (when (or (member :s *keys-pressed*)
                (member :down *keys-pressed*))
            (decf (y (pos e)) (move-speed e)))
      (contain-entity e))

(defmethod contain-entity ((e twohu-entity))
      (when (< (x (pos e)) 1)
            (setf (x (pos e)) 1))
      (when (> (x (pos e)) (- *window-w* (width e)))
            (setf (x (pos e)) (- *window-w* (width e))))
      (when (< (y (pos e)) 1)
            (setf (y(pos e)) 1))
      (when (> (y (pos e)) (- *window-h* (height e)))
            (setf (y (pos e)) (- *window-h* (height e)))))

(defmethod draw-anim-cur ((e twohu-entity))
      (gamekit:draw-image (pos e) (anim-cur e)
                          :origin (vec2 (* (anim-index e) (width e)) (* 2 (height e)))
                          :width (width e)
                          :height (height e)))

(defmethod inc-anim ((e twohu-entity))
      (if (equal (anim-index e) +marisa-anim-frames+)
            (setf (anim-index e) 0)
            (incf (anim-index e))))

(defmethod width ((e twohu-entity))
      (x (rect e)))

(defmethod height ((e twohu-entity))
      (y (rect e)))
