
(in-package :twohu)

(defparameter *window-w* 800)
(defparameter *window-h* 800)
(defparameter *player* nil)
(defparameter *keys-pressed* nil)
(defconstant +player-sprite-h+ 97)
(defconstant +player-sprite-w+ 64)

(gamekit:defgame twohu-game () ()
		 (:viewport-width *window-w*)
		 (:viewport-height *window-h*)
		 (:viewport-title "Twohu"))

(defun run ()
    (gamekit:start 'twohu-game))

(defmethod gamekit:post-initialize ((app twohu-game))
  (loop for key in '(:z :y :w :s :a :d :left :right :up :down)
     do (bind-movement-button key))
  (init-game))

(defun init-game ()
    (make-player))

(defmethod gamekit:draw ((this twohu-game))
    (draw-entity *player*))

(defmethod gamekit:act ((this twohu-game))
  (update-pos *player*))

(defun bind-movement-button (b)
    "Bind handler functions for a button b for pressed/released events"
  (gamekit:bind-button b :pressed
		       (lambda ()
			 (push b *keys-pressed*)))
  (gamekit:bind-button b :released
		       (lambda ()
			 (setq *keys-pressed* (remove b *keys-pressed*)))))

(gamekit:register-resource-package :twohu.res
    "/home/fffere/Dev/2hu-demo/res/")

(gamekit:define-image twohu.res::marisa-idle "marisa-sheet.png")

(defun make-player ()
    (setf *player* (make-instance 'twohu-entity
                            :hp 3
                            :dead nil
                            :anim-cur 'twohu.res::marisa-idle
                            :rect (vec2 +player-sprite-w+ +player-sprite-h+)
                            :pos (vec2 (* *window-w* .75) (* *window-h* .20)))))
