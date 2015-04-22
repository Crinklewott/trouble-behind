;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :io.github.thingywhat.prettify
  (:use :common-lisp :asdf)
  (:export :fluff-word-p
           :a/an
           :stylize-string
           :stylize-list
           :princ-stylized-list))

(defpackage :io.github.thingywhat.trouble-behind
  (:use :common-lisp :asdf :io.github.thingywhat.prettify))

(in-package :io.github.thingywhat.trouble-behind)

(defsystem trouble-behind
  :name "Trouble Behind"
  :version "0.0"
  :author "thingywhat"
  :serial t
  :components ((:module prettify
                        :serial t
                        :pathname "lib"
                        :components ((:file "prettify")))
               (:module game
                        :serial t
                        :pathname "game"
                        :components ((:file "classes")
                                     (:file "state")
                                     (:file "map")
                                     (:file "player-commands")
                                     (:file "ai")
                                     (:file "map-utility")
                                     (:file "player")))))

