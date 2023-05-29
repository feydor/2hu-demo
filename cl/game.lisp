;;;; game.lisp - The base level functions and event loop

(in-package :cl-user)
(defpackage #:2hu
  (:use :cl :trivial-gamekit)
  (:import-from :2hu.entity
                #:make-entity
                #:make-shot
                #:entity-pos-copy
                #:entity-draw
                #:entity-update
                #:entity-dead)
  (:export #:run))
(in-package :2hu)

(defpackage #:2hu.res
  (:use :cl))

(defconstant +window-w+ 800)
(defconstant +window-h+ 800)
(defconstant +player-sprite-h+ 97)
(defconstant +player-sprite-w+ 64)
(defconstant +player-frames+ 8)
(defconstant +player-move-speed+ 9)
(defconstant +player-shot-speed+ 16)

(defparameter *player* nil)
(defparameter *shots* '())
(defparameter *keys-pressed* nil)

(gamekit:defgame 2hu-game () ()
  (:viewport-width +window-w+)
  (:viewport-height +window-h+)
  (:viewport-title "2hu"))

(defun run ()
  "Start 2hu."
  (gamekit:start '2hu-game))

(defmethod gamekit:post-initialize ((app 2hu-game))
  "Bind the used keys and init the game."
  (loop for key in '(:w :s :a :d :left :right :up :down)
        do (bind-movement-button key))
  (gamekit:bind-button :escape :pressed
                       (lambda () (gamekit:stop)))
  (gamekit:bind-button :x :pressed
                       (lambda () (push (make-shot (entity-pos-copy *player*)
                                                   (vec2 0 +player-shot-speed+)
                                                   '2hu.res::player-shot
                                                   (image-size '2hu.res::player-shot)
                                                   1)
                                        *shots*)))
  (init-game))

(defun init-game ()
  (make-player))

(defmethod gamekit:draw ((this 2hu-game))
  ; first draw the background
  (gamekit:draw-rect (vec2 0 0) +window-w+ +window-h+
                     :fill-paint (vec4 0.75 0.75 0.5 1)
                     :stroke-paint (vec4 0 0.75 0.5 1)
                     :thickness 30)
  (loop for shot in *shots*
        do (entity-draw shot))
  (entity-draw *player*))

(defmethod gamekit:act ((this 2hu-game))
  (loop for shot in *shots*
        do (progn
             (entity-update shot nil +window-w+ +window-h+)
             (when (entity-dead shot)
               (setq *shots* (remove shot *shots*)))))
  (entity-update *player* *keys-pressed* +window-w+ +window-h+))

(defun bind-movement-button (b)
    "Bind handler functions for a button b for pressed/released events"
  (gamekit:bind-button b :pressed
		       (lambda ()
			 (push b *keys-pressed*)))
  (gamekit:bind-button b :released
		       (lambda ()
			 (setq *keys-pressed* (remove b *keys-pressed*)))))

;; TODO: Move this to another file
(gamekit:register-resource-package :2hu.res "/home/fffere/Dev/2hu-demo/res/")
(gamekit:define-image 2hu.res::marisa-idle "marisa-sheet.png")
(gamekit:define-image 2hu.res::player-shot "marisa-shot.png")

(defun make-player ()
  (setf *player* (make-entity (vec2 (* +window-w+ .50) (* +window-h+ .20))
                              :anim '2hu.res::marisa-idle
                              :rect (vec2 +player-sprite-w+ +player-sprite-h+)
                              :anim-frames +player-frames+
                              :velocity (vec2 +player-move-speed+ +player-move-speed+)
                              :hp 3)))

(defun image-size (image)
  (vec2 (image-width image)
        (image-height image)))
