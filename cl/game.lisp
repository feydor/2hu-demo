;;;; game.lisp - The base level functions and event loop

(in-package :cl-user)
(defpackage :2hu
  (:use :cl :trivial-gamekit :common-functions)
  (:import-from :2hu.entity
                :make-entity
                :make-shot
                :entity-pos-clone
                :entity-draw
                :entity-print
                :entity-update
                :entity-update-all
                :entity-dead-p)
  (:import-from :2hu.entity.player
                :make-player)
  (:import-from :2hu.entity.enemy
                :make-enemy)
  (:import-from :2hu.emitter
   :make-emitter-basic
                :emitter-update)
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
(defconstant +enemy-frames+ 5)
(defconstant +enemy-sprite-w+ 30)
(defconstant +enemy-sprite-h+ 26)

(defparameter *player* nil)
(defparameter *emitter* (make-emitter-basic (/ +window-w+ 2)
                                            (/ +window-h+ 2)))
(defparameter *shots* (make-dynamic-array))
(defparameter *enemies* (make-dynamic-array))
(defparameter *keys-pressed* nil)
(defparameter *ticks* 0)

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
                       (lambda () (vector-push-extend
                                   (make-shot (entity-pos-clone *player*)
                                              (vec2 0 +player-shot-speed+)
                                              '2hu.res::player-shot
                                              (image-size '2hu.res::player-shot)
                                              1)
                                   *shots*)))
  (init-game))

(defun init-game ()
  "Create initial entities."
  (setq *player* (make-player (vec2 (* +window-w+ .50) (* +window-h+ .20))
                              +player-move-speed+
                              +player-move-speed+
                              +player-sprite-h+
                              +player-sprite-w+
                              '2hu.res::marisa-idle
                              +player-frames+))
  (vector-push-extend (make-enemy (vec2 (/ +window-w+ 2) (- +window-h+ 100))
                    (vec2 0 -3)
                    :anim '2hu.res::enemy-idle
                    :anim-frames +enemy-frames+
                    :rect (vec2 30 26))
        *enemies*))

(defmethod gamekit:draw ((this 2hu-game))
  ; first draw the background
  (gamekit:draw-rect (vec2 0 0) +window-w+ +window-h+
                     :fill-paint (vec4 0.75 0.75 0.5 1)
                     :stroke-paint (vec4 0 0.75 0.5 1)
                     :thickness 30)
  (loop for shot across *shots*
        do (entity-draw shot))
  (loop for enemy across *enemies*
        do (entity-draw enemy))
  (entity-draw *player*))


(defmethod gamekit:act ((this 2hu-game))
  (entity-update-all *enemies* *keys-pressed* +window-w+ +window-h+)
  (entity-update-all *shots* *keys-pressed* +window-w+ +window-h+)
  (emitter-update *emitter* *enemies* *player* *ticks*
                  '2hu.res::enemy-idle +enemy-frames+ 30 26)
  (entity-update *player* *keys-pressed* +window-w+ +window-h+)
  ;;(print-elements-of-list *shots* 'entity-print)
  (incf *ticks*))

(defun bind-movement-button (b)
  "Bind handler functions for a button b for pressed/released events"
  (gamekit:bind-button b :pressed
		       (lambda ()
			 (push b *keys-pressed*)))
  (gamekit:bind-button b :released
		       (lambda ()
			 (setq *keys-pressed* (remove b *keys-pressed*)))))

;; TODO: Move this to another file
(gamekit:register-resource-package :2hu.res "/home/fffere/.roswell/local-projects/2hu-demo/res/")
(gamekit:define-image 2hu.res::marisa-idle "marisa-sheet.png")
(gamekit:define-image 2hu.res::player-shot "marisa-shot.png")
(gamekit:define-image 2hu.res::enemy-idle "enemy-idle.png")

(defun image-size (image)
  (vec2 (image-width image)
        (image-height image)))
