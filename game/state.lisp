(in-package :io.github.thingywhat.trouble-behind)

;; Dynamically loads things from a map
(defmacro defmap (map entry getter-name fetcher-name)
  "Defines a get-all-<things> function to fetch the alist for some
specific thing from the loaded map and get-<things> to fetch an
individual item"
  `(let ((,entry (cadr (assoc ',entry ,map))))
     (defun ,fetcher-name (key)
       (cdr (assoc key ,entry)))
     (defun (setf ,fetcher-name) (new-value key)
       (setf ,entry (mapcar (lambda (item) (if (eq key (car item))
                                               (cons (car item) new-value)
                                               item))
                            ,entry)))
     (defun ,getter-name ()
       ,entry)
     (defun (setf ,getter-name) (new-value)
       (setf ,entry new-value))))


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
     "A list of NPC objects")

   ;; Load each type of map item we care about and creates a whole mess
   ;; of utility functions
   (defmap *map* nodes get-nodes get-node)
   (defmap *map* edges get-all-edges get-edges)
   (defmap *map* items get-items get-item)
   (defmap *map* item-details get-item-details get-item-detail)
   (defmap *map* events get-event-list get-event)
   (defmap *map* spunk-messages get-spunk-messages get-spunk-message)

(defun pick (list)
  "Gets a random item from the passed in list"
  (nth (random (length list)) list))
