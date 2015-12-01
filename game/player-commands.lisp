(in-package :io.github.thingywhat.trouble-behind)

;; Player location-oriented functions
(defun look ()
  "Outputs what the player sees around them."
  (append
   (car (get-node (actor-location *player*)))
   (describe-edges (get-edges (actor-location *player*)))
   (describe-items-at-location (get-item-details) (actor-location *player*))
   (describe-npcs-at-location (actor-location *player*))))

(defun look-at (target)
  "Gets the player to look at some target if they can see it."
  (let* ((player-location (actor-location *player*))
         (npc (can-see-npc target player-location)))
    (if npc
        (npc-description npc)
        (if (can-see-item target player-location)
            (car (get-item target))
            `(you cannot see any ,target around here.)))))

(defun walk (direction)
  "Makes the player walk a specific direction if possible."
  (let ((edge (assoc direction (get-edges (actor-location *player*)))))
    (if edge
        (progn
          (setf (player-hidden *player*) 0)
          (if (zerop (random (1+ (length (player-removed-clothes *player*)))))
              (let ((blocking (find-if (lambda (npc)
                                         (and (npc-blocking npc)
                                              (eq direction (npc-direction npc))))
                                       *npcs*)))
                (if blocking
                    `(,(actor-name blocking) blocks your path!)
                    (progn (setf (actor-location *player*) (cadr edge))
                           (look))))
              `(your pulled-down
                     ,(car (player-removed-clothes *player*))
                     trips you! you should probably pull them up.)))
        `(you cannot see anywhere ,direction of here.))))

(defun inventory ()
  "Returns the player's inventory"
  (loop for item in (get-items)
     when (eq (item-location (car item)) 'inventory)
     collect (car item)))

(defun pickup (item)
  "Lets the player pick up an item and put it in their inventory."
  (if (can-see-item item (actor-location *player*))
      (if (member item (inventory))
          '(you already have that.)
          (let ((excuse (cadr (get-item item))))
            (if (not excuse)
                (progn (push (cons item 'inventory) *item-locations*)
                       `(you pick up the ,item))
                excuse)))
      `(you cannot see ,(a/an item) ,item from here.)))

(defun drop (item)
  "Lets the player drop an item at their current location"
  (if (member item (inventory))
      (progn (push (cons item (actor-location *player*)) *item-locations*)
             `(you drop the ,item on the floor.))
      `(you dont have that.)))

(defun hide (location item)
  "Lets the player hide somewhere."
  (if (eq (actor-location *player*) (car item))
      (progn (unless (find (actor-location *player*) (mapcar #'actor-location *npcs*))
               (setf (player-hidden *player*) (caddr item)))
             `(you are now hiding ,location the ,(cadr item)))
      `(you cannot see that...)))
