;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :io.github.thingywhat.trouble-behind
  (:use :common-lisp :asdf))

(in-package :io.github.thingywhat.trouble-behind)

(defsystem trouble-behind
  :name "Trouble Behind"
  :version "0.0"
  :author "thingywhat"
  :serial t
  :components ((:file "game/classes")
               (:file "game/output")
               (:file "game/state")
               (:file "game/map")
               (:file "game/player-commands")
               (:file "game/ai")
               (:file "game/map-utility")
               (:file "game/player")))
