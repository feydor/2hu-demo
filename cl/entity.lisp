;;;; entity.lisp

(defclass entity ()
    ((pos :initarg :pos
          :initform (vec2 0.0 0.0)
          :accessor pos)
     (rect :initarg :rect
            :initform (vec2 64 64)
            :accessor rect)
     (dead :initarg :alive
           :initform nil
           :accessor dead)
     (idle-anim :initarg :idle-anim
                :initform nil
                :accessor idle-anim)
     (hp :initarg :hp
         :initform 1
         :accessor hp)))

