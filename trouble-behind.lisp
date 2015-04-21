(load "load.lisp")
(in-package :io.github.thingywhat.trouble-behind)

;; Loading external information first
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
            (make-instance 'npc :name (car npc) :location (cadr npc)))
	  (cadr (assoc 'npcs *map*)))
  "A list of NPC objects")

;; Dynamically loads things from the map
(defmacro defmap (entry getter-name fetcher-name)
  "Defines a get-all-<things> function to fetch the alist for some
specific thing from the loaded map and get-<things> to fetch an
individual item"
  `(let ((,entry (cadr (assoc ',entry *map*))))
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


;; Load each type of map item we care about and creates a whole mess
;; of utility functions
(defmap nodes get-nodes get-node)
(defmap edges get-all-edges get-edges)
(defmap items get-items get-item)
(defmap item-details get-item-details get-item-detail)
(defmap events get-event-list get-event)

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

(defun can-see (item location)
  "Checks if you can see an item currently"
  (let ((loc (item-location item)))
    (or (eq location loc)
	(eq 'inventory loc))))

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

;; Player location-oriented functions
(defun look ()
  "Outputs what the player sees around them."
  (append
   (car (get-node (actor-location *player*)))
   (describe-edges (get-edges (actor-location *player*)))
   (describe-items-at-location (get-item-details) (actor-location *player*))
   (describe-npcs-at-location (actor-location *player*))))

(defun look-at (item)
  "Gets the player to look at an item if they can see it."
  (if (can-see item (actor-location *player*))
      (car (get-item item))
      `(you cannot see any ,item around here.)))

(defun walk (direction)
  "Makes the player walk a specific direction if possible."
  (let ((edge (assoc direction (get-edges (actor-location *player*)))))
    (if edge
	(progn (setf (actor-location *player*) (cadr edge))
	       (look))
	`(i cannot see anywhere ,direction of here.))))

(defun inventory ()
  "Returns the player's inventory"
  (loop for item in (get-items)
     when (eq (item-location (car item)) 'inventory)
     collect (car item)))

(defun pickup (item)
  "Lets the player pick up an item and put it in their inventory."
  (if (can-see item (actor-location *player*))
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


;; AI functions!
(let ((distance-hashes (make-hash-table)))
  (defun clear-distance-hashes ()
    "Clears the cache of distance-hashes"
    (setf distance-hashes (make-hash-table)))

  (defun make-distance-hash (start)
    "Creates a hash table that lists the shortest distance to each node
from the staring node passed in."
    (let ((visited (make-hash-table)))
      (labels ((neighbors (node)
                 (mapcar #'cadr (get-edges node)))
               (traverse (node depth)
                 (let ((current (gethash node visited)))
                   (unless (and current (< current depth))
                     (setf (gethash node visited) depth)
                     (mapc (lambda (node)
                             (traverse node (1+ depth)))
                           (neighbors node))))))
        (traverse start 0))
      visited)))

(defun get-nodes-within-range (node max-distance)
  "Returns an alist of the node names to distances within a set
distance from a given source node."
  (let ((hash (make-distance-hash node)))
    (loop for room being the hash-keys of hash
       for distance being the hash-values of hash
       when (>= max-distance distance) collect (cons room distance))))

(defun get-npcs-within-range (node max-distance)
  "Returns a list of NPCs within the specified distance of the
passed in node."
  (mapcan (lambda (node)
            (mapcan (lambda (npc)
                      (when (eq (car node) (actor-location npc))
                        (list npc)))
                    *npcs*))
          (get-nodes-within-range node max-distance)))

(let ((cache (make-hash-table :test #'equal)))
  (defun get-path (start end &optional retry)
    "Gets the shortest path from one node to another."
    (or (and (null retry) (gethash (cons start end) cache))
	(setf (gethash (cons start end) cache)
	      (loop with hash = (make-distance-hash end)
		 for distance from (or (gethash start hash) 0) downto 0
		 and node = start then
		   (cdr (assoc (1- distance)
			       (mapcar (lambda (node)
					 (cons (gethash (cadr node) hash)
					       (cadr node)))
				       (get-edges node)) :test #'equal))
		 collect node)))))

(defun update-random-path ()
  "Updates a random path so AIs can slowly \"learn\" new paths
  eventually if they are more efficeint than before."
  (flet ((random-node ()
	   (let ((nodes (mapcar #'car (get-nodes))))
	     (nth (random (length nodes)) nodes))))
    (get-path (random-node) (random-node) t)))


;; NPC command and AI implementation details
(defun npc-alert-players-in-range (location)
  "Alerts the players of the sound of NPCs approaching them from up to
  3 nodes away."
  (mapc (lambda (node)
          (when (eq (car node) (actor-location *player*))
            (princ-stylized-list
             (case (cdr node)
               (1 '(you hear footsteps just outside the room.))
               (2 '(you hear footsteps.))
               (3 '(you hear the faint sound of footsteps.))))))
        (remove-if (lambda (node) (zerop (cdr node)))
                   (get-nodes-within-range location 3))))

(defun display-walk (source npc)
  "Outputs what a player would see given their current location if an
NPC walked from some location to their current location."
  (flet ((get-direction (source destination)
	   (caar (remove-if-not (lambda (x) (eq destination (cadr x)))
				(get-edges source)))))
    (let ((destination (actor-location npc)))
      (if (eq (actor-location *player*) source)
	  (unless (eq (actor-location *player*) destination)
	    (let ((direction (get-direction source destination)))
	      (princ-stylized-list
	       `(you see ,(actor-name npc) walk to the ,direction))))
          (progn
            (npc-alert-players-in-range destination)
            (when (eq (actor-location *player*) destination)
              (let ((direction (get-direction destination source)))
                (princ-stylized-list
                 `(,(actor-name npc) enters from the ,direction)))))))))

(defgeneric npc-ai (npc motive)
  (:documentation "The AI that controls the passed in NPC each
  turn; Whis is done will do is based on its motive."))
(defgeneric npc-alert (npc location)
  (:documentation "How the AI reacts to being alerted of an event."))

(defun npc-follow-path (npc)
  "Gets an NPC to continue following the path it has."
  (let ((source (actor-location npc)))
    (setf (actor-location npc) (pop (npc-path npc)))
    (display-walk source npc)))

(defmethod npc-ai (npc motive)
  "The basic NPC AI, used when no matching AI is found for the
current motive:
This AI will make the NPC randomly move from one room to another every
once in a while... Or if a path is set, follow it."
  (let ((source (actor-location npc)))
    (if (npc-path npc)
        (npc-follow-path npc)
        (let ((neighbors (mapcar #'cadr (get-edges (actor-location npc)))))
          (when (and (zerop (random 3))
                     (not (zerop (length neighbors))))
            (setf (actor-location npc)
                  (nth (random (length neighbors)) neighbors)))
          (display-walk source npc)))))

(defmethod npc-ai (npc (motive (eql 'find-player)))
  "The NPC motive code for finding the player. (Currently in a dumb
and psychic way)"
  (if (npc-path npc)
      (npc-follow-path npc)
      (setf (npc-path npc)
            (get-path (actor-location npc) (actor-location *player*))))
  (when (eq (actor-location npc) (actor-location *player*))
    (pop (npc-motives npc))))

(defmethod npc-ai (npc (motive (eql 'grab-player)))
  "The NPC motive code for when they wish to grab the player."
  (if (eq (actor-location npc) (actor-location *player*))
      (unless (or (zerop (random 2))
                  (eq (item-location 'player)
                      (actor-inventory npc)))
        (push (cons 'player (actor-inventory npc)) *item-locations*)
        (princ-stylized-list `(,(actor-name npc) grabs ahold of you!)))
      (push 'find-player (npc-motives npc))))

(defmethod npc-ai (npc (motive (eql 'investigate)))
  "Investigates the current location, or the location at the end of
their path."
  (flet ((investigate-event (event)
           (unless (find event (npc-seen npc) :test #'equal)
             (push event (npc-seen npc))
             (incf (npc-anger npc) (cadr (get-event-details event))))))
  (when (npc-path npc)
    (npc-follow-path npc))
  (mapc #'investigate-event (get-events-complete-at (actor-location npc)))))

(defun update-npcs ()
  "Updates all of the currently active NPCs after they completed their
tasks."
  (flet ((npc-ai-action (npc)
           (npc-ai npc (car (npc-motives npc)))))
    (mapc #'npc-ai-action *npcs*)))

(defun npc-alert-in-range (location distance)
  "Alerts any NPCs within a certain distance of some location of an
event happening"
  (mapc (lambda (npc) (npc-alert npc location))
        (get-npcs-within-range 'hallway distance)))

(defun npc-goto (npc location)
  "Tells an NPC they should go to a certain location."
  (setf (npc-path npc) (get-path (actor-location npc) location)))

(defmethod npc-alert ((npc npc) location)
  "Basic NPC alert AI... When alerted, the NPC goes to investigate the
node the event happened at."
  (npc-goto npc location))

;; Map utility functions
(defun new-location-description (description &optional place)
  "Sets a new description for the passed-in location"
  (setf (car (get-node (or place (actor-location *player*)))) description))

(defun item-is-now-at (item place)
  "Moves an item to some place."
  (push (cons item place) *item-locations*))

(defun connect-places (place1 direction1 place2 direction2 item)
  "Connects two places with an item."
    (push (list direction1 place2 item) (get-edges place1))
    (push (list direction2 place1 item) (get-edges place2))
    (clear-distance-hashes))

;; Advaced metaprogramming thingies
(defmacro when-player (&rest arg-list)
  "Checks if the player meets certain conditions in plain english. For example:
\(when-player has blanket)
...is valid, and will check if the player has a blanket in their
inventory...

You can chain together statements with \"and\" as well, so the
following works too:
\(when-player has blanket and is in your-bedroom)

Any words the macro doesn't understand are simply ignored.

Valid words are:
- in, at <place>
- has, holds <item>
- see, sees <thing>
- already <special command>"
  (with-slots (location) *player*
    (labels ((parse (args acc)
               (let ((current (car args)))
                 (when current
                   (case current
                     ((in at)
                      (cons acc (cons `(eq ',(cadr args) ',location)
                                    (parse (cddr args) acc))))
                   ((has holds)
                    (cons acc (cons `(member ',(cadr args) (inventory))
                                    (parse (cddr args) acc))))
                   ((already)
                    (cons acc (cons `(special-command-run-p ',(cadr args))
                                    (parse (cddr args) acc))))
                   ((see sees)
                    (cons acc (cons `(can-see ',(cadr args) ',location)
                                    (parse (cddr args) acc))))
                   (and (cdr (parse (cdr args) '())))
                   (otherwise (parse (cdr args) '())))))))
    (parse (remove-if #'fluff-word-p arg-list) 'and))))

(defun special-command-run-p (command)
  "Checks if a special command has already run successfully."
  (assoc (remove-if #'fluff-word-p command) *events-complete* :test #'equal))

(defun run-user-event (points range message &optional code)
  "Runs a special type of form created by the user.
Accepts four arguments:

  points  - The number of trouble-points this event will give \(Or take
            away\) from the player
  range   - How far away is this event noticable from?
  message - The message the user sees when this event is run.
  code    - An optional argument that will allow arbitrary Lisp code to
            run when this event occurs. This is done in a small
            user-namespace where \"location\" will be replaced with
            the actual player location.

Currently only used in special-command, but can be used as a way to
add user-scripting to user-files.."
  (with-slots (location trouble-points) *player*
    (when code
      (mapc #'eval (mapcar (lambda (form)
                             (substitute `(quote ,location) 'location form))
                           code)))
    (incf trouble-points points)
    (npc-alert-in-range location range)
    message))
  
(defun special-command (input)
  "Runs a command configured in the map"
    (let* ((event (get-event (car input)))
           (args (cdr input))
           (location (actor-location *player*))
           (form (assoc args event :test #'equal)))
      (if form
          (if (not (special-command-run-p input))
              (if (eval
                   (substitute `(quote ,location) 'location (fourth form)))
                  (progn
                    (push
                     (list input (actor-location *player*)) *events-complete*)
                    (run-user-event
                     (second form) (third form) (fifth form) (sixth form)))
                  '(you cannot do that.))
              '(you already did that.))
          '(huh?))))


;; Game REPL functions
(defun game-eval (&rest input)
  "Evaluates user input from (read) in a controlled manner and
performs the respective game commands passed in."
  (let* ((input (car input))
	 (command (car input)))
    (case command
      ((inventory i items)
       (or (inventory) '(nothing)))
      ((look inspect describe l)
       (if (cdr input)
           (look-at (cadr input))
           (look)))
      ((get pickup)
       (if (cdr input)
           (pickup (cadr input))
           '(pick up what?)))
      ((drop leave)
       (if (cdr input)
           (drop (cadr input))
           '(drop what?)))
      ((walk go run)
       (if (cdr input)
           (walk (cadr input))
           '(walk where?)))
      ((n e s w ne nw se sw)
       (game-eval (case command
                  (n '(north))
                  (e '(east))
                  (s '(south))
                  (w '(west))
                  (ne '(northeast))
                  (nw '(northwest))
                  (se '(southeast))
                  (sw '(southwest)))))
      (otherwise
       (cond
         ((member command
                  (append '(north east south west northeast northwest
                            southeast southwest)
                          (mapcar #'car
                                  (get-edges (actor-location *player*)))))
          (walk command))
         (t (special-command input)))))))

(defun handle-player (input)
  "Handles the actions a player can take in their given situation."
  (flet ((holding-player-p (npc)
           (eq (item-location 'player) (actor-inventory npc))))
    (let ((holding-npc (car (remove-if-not #'holding-player-p *npcs*))))
      (if (null holding-npc)
          (princ-stylized-list (game-eval (remove-if #'fluff-word-p input)))
          (if (eq (car input) 'struggle)
              (if (zerop (random 10))
                  (progn
                    (push (cons 'player 'free) *item-locations*)
                    (princ-stylized-list '(you get away!)))
                  (princ-stylized-list
                   '(you struggle... but it is fruitless.)))
              (princ-stylized-list
               `(you "can't" move! ,(actor-name holding-npc) is holding onto
                     you tightly! try to struggle to get away!)))))))

;; Main game loop
(defun game-loop ()
  "Loops through user input passing it to game-eval and stylyzing the
output."
  (loop for input = '(look)
     then (read-from-string (concatenate 'string "(" (read-line) ")"))
     when (eq (car input) 'quit)
     return t
     do (progn
          (handle-player input)
          (fresh-line)
          (update-npcs)
	  (fresh-line))))
