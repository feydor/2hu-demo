
(defparameter *window-w* 800)
(defparameter *window-h* 800)
(defparameter *player* nil)

(gamekit:defgame twohu-game () ()
		 (:viewport-width *width*)
		 (:viewport-height *height*)
		 (:viewport-title "Twohu"))

(defmethod gamekit:draw ((this twohu-game))
    (draw-player))

(defmethod gamekit:post-initialize ((app twohu-game))
;   (loop for key in '(:z :y :w :s :a :d :left :right :up :down)
;      do (bind-movement-button key))
  (init-game))

(defun init-game ()
    (make-player))

(defun make-player ()
    (progn
         (gamekit:define-image twohu::marisa-idle "res/marisa-sheet.png")
         (setf *player* (make-instance 'twohu-entity
                                  :hp 3
                                  :dead nil
                                  :rect (vec2 64 64)
                                  :pos (vec2 (* *width* .75) (* *height* .20))))))

(defun draw-player ()
    (gamekit:draw-image (pos *player*) 'twohu::marisa-idle
                        :origin (gamekit:vec2 0 0)
                        :width (x (rect *player*))
                        :height (y (rect *player*))))


(gamekit:start 'twohu-game)
