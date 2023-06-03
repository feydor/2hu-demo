;;;; animation.lisp
;;;; Animation object

(in-package :cl-user)
(defpackage :2hu.animation
  (:use :cl :common-functions)
  (:import-from :trivial-gamekit
                :vec2
                :x :y)
  (:export #:make-animation))
(in-package :2hu.animation)

(defclass animation ()
  ((sheet :accessor animation-sheet
          :initarg :sheet
          :documentation "The Animation's sheet.")
   (horizontal-frames :accessor animation-h-frames
                      :type integer
                      :initform 1
                      :initarg :horizontal-frames
                      :documentation "The # of horizontal frames in the sheet.")
   (vertical-frames :accessor animation-v-frames
                    :type integer
                    :initform 0
                    :initarg :vertical-frames
                    :documentation "The # of vertical frames in the sheet.")
   (clip :accessor animation-clip
         :initform (vec2 0 0)
         :initarg :clip
         :documentation "The rectangle used to access the given frame from the sheet.")
   (clip-width :accessor clip-width
               :type integer
               :initform 64
               :initarg :clip-width
               :documentation "The width of the clip.")
   (clip-height :accessor clip-height
                :type integer
                :initarg :clip-height
                :documentation "Te height of the clip."))
  (:documentation "Animation encapsulates all animation related data."))

(defun make-animation (animation-sheet &key horizontal-frames vertical-frames frame-height frame-width)
  "Make an animation with given sheet."
  (make-instance 'animation
                 :sheet animation-sheet
                 :horizontal-frames horizontal-frames
                 :vertical-frames vertical-frames
                 :clip (vec2 frame-width frame-height)))

(defmethod draw-current-animation ((a animation) position)
  (gamekit:draw-image position (animation-sheet a)
                      :origin (animation-clip a)
                      :width ()))

(defmethod increment-animation-horizontally ((a animation)))
