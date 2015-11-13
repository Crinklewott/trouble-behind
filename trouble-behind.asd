;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem trouble-behind
  :name "Trouble Behind"
  :version "0.0"
  :author "thingywhat"
  :serial t
  :components ((:file "packages")
               (:module prettify
                        :serial t
                        :pathname "lib"
                        :components ((:file "prettify")))
               (:module game
                        :serial t
                        :pathname "game"
                        :components ((:file "classes")
                                     (:file "state")
                                     (:file "map")
                                     (:file "user-input")
                                     (:file "player-commands")
                                     (:file "ai")
                                     (:file "map-api")
                                     (:file "player")))))
