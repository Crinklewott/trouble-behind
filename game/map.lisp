(in-package :io.github.thingywhat.trouble-behind)

;; Querying
(defun item-location (item)
  "Fetches the location of an item"
  (cdr (assoc item *item-locations*)))

(defun item-moved-p (item)
  "Returns t if an item has moved from its original location, nil
otherwise."
  (loop for i in *item-locations*
     when (eq item (car i)) collect i into acc
     when (< 1 (length acc)) return t))

(defun can-see-item (item location)
  "Checks if you can see an item currently"
  (let ((loc (item-location item)))
    (or (eq location loc)
	(eq 'inventory loc))))

(defun can-see-npc (target-name target-location)
  "Checks and returns any NPC you can see in the passed in location
given the passed in name"
  (loop for npc in *npcs*
     when (with-slots (name location) npc
            (and (eq target-name name) (eq target-location location)))
     return npc))

(defun can-see (target location)
  "Checks if you can see either an NPC or an item at the given
location"
  (or (can-see-npc target location)
      (can-see-item target location)))

(defun get-event-details (event)
  "Fetches the details of an event action list."
  (assoc (cdr event) (get-event (car event)) :test #'equal))

(defun get-events-complete-at (location)
  "Gets all of the events the player has completed at a given location."
  (mapcar #'car (remove-if-not
                 (lambda (event) (eq (cadr event) location))
                 *events-complete*)))


;; Game description functions
(defun describe-path (edge)
  "Describes a path connecting two nodes."
  (let ((place (caddr edge))
	(direction (car edge)))
    (list 'there 'is (a/an place) place 'to 'the direction 'of 'here.)))

(defun describe-edges (edges)
  "Describes all of the exits from the current node."
  (mapcan #'describe-path edges))

(defun describe-items-at-location (item-details location)
  "Describes the items at the passed-in location."
  (mapcan (lambda (item-detail)
	    (copy-list
	     (let ((item (car item-detail)))
	       (when (eq location (item-location item))
		 (if (item-moved-p item)
		     `(there is ,(a/an item) ,item on the floor here.)
		     (cadadr item-detail))))))
	  item-details))


(defun describe-npcs-at-location (location)
  "Describes all of the NPCs in the past in list that are at the
passed-in location."
  (mapcan (lambda (npc) (list (actor-name npc) 'is 'in 'the 'room 'with 'you.))
          (remove-if-not (lambda (npc) (eq (actor-location npc) location))
                         *npcs*)))
