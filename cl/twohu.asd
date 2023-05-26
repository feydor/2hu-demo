;;;; twohu.asd

(asdf:defsystem :twohu
    :description "Twohu Danmaku Demo"
    :author "feydor"
    :license "MIT"
    :version "0.0.1"
    :serial t
    :depends-on (trivial-gamekit)
    :components ((:file "package")
                 (:file "game")
                 (:file "entity")))
