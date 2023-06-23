;;;; twohu.asd

(asdf:defsystem 2hu
    :description "Twohu Danmaku Demo"
    :author "feydor"
    :license "MIT"
    :version "0.0.1"
    :serial t
    :depends-on (:trivial-gamekit :log4cl)
    :components ((:file "common")
                 (:file "entity")
                 (:file "shot")
                 (:file "emitter")
                 (:file "game")))
