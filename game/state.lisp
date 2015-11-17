(in-package :io.github.thingywhat.trouble-behind)

(funcall
 (defun reset-state ()
   "Resets the state of the world."
   (defparameter *map*
     (with-open-file (nodes "map.lmap" :direction :input)
       (read nodes))
     "The map that contains all of the location nodes as well as the
  edges that connect them.")

   (defparameter *item-locations*
     (mapcar (lambda (x) (cons (car x) (caadr x)))
             (cadr (assoc 'item-details *map*)))
     "An alist containing the locations of items")

   (defparameter *player*
     (make-instance 'player :location (cadr (assoc 'player-location *map*)))
     "The player.")

   (defparameter *events-complete* '()
     "A list of events that have been successfully completed.")

   (defparameter *npcs*
     (mapcar (lambda (npc)
               (make-instance 'npc :name (car npc)
                              :location (cadr npc)
                              :punishment-begin-messages (caddr npc)
                              :punishment-messages (cadddr npc)))
             (cadr (assoc 'npcs *map*)))
     "A list of NPC objects")))

(defun pick (list)
  "Gets a random item from the passed in list"
  (nth (random (length list)) list))
