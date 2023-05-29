;;;; game.lisp - The base level functions and event loop

(in-package :cl-user)
(defpackage #:2hu
  (:use :cl :trivial-gamekit)
  (:export #:run))
(in-package :2hu)

(defpackage #:2hu.res
  (:use :cl))

(defparameter *window-w* 800)
(defparameter *window-h* 800)
(defparameter *player* nil)
(defparameter *keys-pressed* nil)
(defconstant +player-sprite-h+ 97)
(defconstant +player-sprite-w+ 64)

(gamekit:defgame 2hu-game () ()
  (:viewport-width *window-w*)
  (:viewport-height *window-h*)
  (:viewport-title "2hu"))

(defun run () 
  (gamekit:start '2hu-game))

(defmethod gamekit:post-initialize ((app 2hu-game))
  (loop for key in '(:z :y :w :s :a :d :left :right :up :down)
     do (bind-movement-button key))
  (init-game))

(defun init-game () 
  (make-player))

(defmethod gamekit:draw ((this 2hu-game)) 
  (2hu.entity:entity-draw *player*))

(defmethod gamekit:act ((this 2hu-game))
  (2hu.entity:entity-update-pos *player* *keys-pressed* *window-w* *window-h*))

(defun bind-movement-button (b)
    "Bind handler functions for a button b for pressed/released events"
  (gamekit:bind-button b :pressed
		       (lambda ()
			 (push b *keys-pressed*)))
  (gamekit:bind-button b :released
		       (lambda ()
			 (setq *keys-pressed* (remove b *keys-pressed*)))))

(gamekit:register-resource-package :2hu.res "/home/fffere/Dev/2hu-demo/res/")
(gamekit:define-image 2hu.res::marisa-idle "marisa-sheet.png")

(defun make-player ()
    (setf *player* (make-instance '2hu.entity:entity
                            :hp 3
                            :dead nil
                            :anim-cur '2hu.res::marisa-idle
                            :rect (vec2 +player-sprite-w+ +player-sprite-h+)
                            :pos (vec2 (* *window-w* .75) (* *window-h* .20)))))
