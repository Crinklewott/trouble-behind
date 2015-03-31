;; Classes
(defclass npc ()
  ((name
    :documentation "The name of the NPC"
    :initarg :name
    :accessor npc-name
    :type string)
   (location
    :documentation "The NPC's current location"
    :initarg :location
    :accessor npc-location
    :type symbol)
   (holding
    :documentation "What the NPC is currently holding... Can be up to
    three things in a list."
    :initarg :holding
    :initform '()
    :accessor npc-holding
    :type list)
   (path
    :documentation "The path that the NPC is attempting to walk."
    :initarg :path
    :initform '()
    :accessor npc-path
    :type list)
   (anger
    :documentation "How mad the NPC currently is."
    :initarg :anger
    :initform 0
    :accessor npc-anger
    :type number)
   (motive
    :documentation "The current motivation of the NPC."
    :initarg :motive
    :initform nil
    :accessor npc-motive
    :type symbol))
  (:documentation "An NPC is anything in the game world that isn't an
  object. Adding an NPC to the *npcs* list below will make the NPC
  automagically update according to their AI implementation."))

;; State
(defparameter *map*
  (with-open-file (nodes "map.lmap" :direction :input)
    (read nodes))
  "The map that contains all of the location nodes as well as the
  edges that connect them.")

(defparameter *player-location*
  (cadr (assoc 'player-location *map*))
  "The current location of the player")

(defparameter *trouble-points* 0
  "The score the player has... Also additively measures how much
trouble the player can be in.")

(defparameter *item-locations*
  (mapcar (lambda (x) (cons (car x) (caadr x)))
	  (cadr (assoc 'item-details *map*)))
  "An alist containing the locations of items")

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

;; Grammar and string output functions
(defun fluff-word-p (word)
  "Returns if the passed in word is a fluff word and should be
ignored."
  (member word '(the at to my a can is out with through)))

(let ((vowels '(#\a #\e #\i #\o #\u #\y)))
  (defun begins-with-vowel (thing)
    "Returns the vowel a word begins with, if it begins with it. nil
otherwise."
    (find (char-downcase (car (coerce thing 'list))) vowels))
  (defun a/an (thing)
    "Returns a or an depending on the grammar used to describe something."
    (if (begins-with-vowel (prin1-to-string thing)) 'an 'a)))

(defun stylize-string (str)
  "Stylizes a string so it prints with correct capitalization and
formatting from a string."
  (labels ((style (char-list caps ver)
	     (when char-list
	       (let ((f (car char-list))
		     (rest (cdr char-list)))
		 (cond
                   (ver (if (char= f #\")
                          (style rest caps nil)
                          (cons f (style rest caps t))))
                   ((char= f #\Space) (cons f (style rest caps ver)))
                   ((char= f #\") (style rest caps t))
		   ((member f '(#\. #\? #\!)) (cons f (style rest t ver)))
		   (caps (cons (char-upcase f) (style rest nil ver)))
		   (t (cons (char-downcase f) (style rest nil ver))))))))
    (coerce (style (coerce str 'list) t nil) 'string)))

(defun stylize-list (list)
  "Stylizes a list as a pretty string."
  (stylize-string (string-trim "()" (prin1-to-string list))))

(defun princ-stylized-list (list)
  "Princs a stylized list with proper capitalization and stuff."
  (fresh-line)
  (princ (stylize-list list))
  (fresh-line))

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
  (mapcan
   (lambda (npc)
     (list (npc-name npc) 'is 'in 'the 'room 'with 'you.))
   (remove-if-not (lambda (npc) (eq (npc-location npc) location)) *npcs*)))

;; Player location-oriented functions
(defun look ()
  "Outputs what the player sees around them."
  (append
   (car (get-node *player-location*))
   (describe-edges (get-edges *player-location*))
   (describe-items-at-location (get-item-details) *player-location*)
   (describe-npcs-at-location *player-location*)))

(defun look-at (item)
  "Gets the player to look at an item if they can see it."
  (if (can-see item *player-location*)
      (car (get-item item))
      `(you cannot see any ,item around here.)))

(defun walk (direction)
  "Makes the player walk a specific direction if possible."
  (let ((edge (assoc direction (get-edges *player-location*))))
    (if edge
	(progn (setf *player-location* (cadr edge))
	       (look))
	`(i cannot see anywhere ,direction of here.))))

(defun inventory ()
  "Returns the player's inventory"
  (loop for item in (get-items)
     when (eq (item-location (car item)) 'inventory)
     collect (car item)))

(defun pickup (item)
  "Lets the player pick up an item and put it in their inventory."
  (if (can-see item *player-location*)
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
      (progn (push (cons item *player-location*) *item-locations*)
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
                      (when (eq (car node) (npc-location npc))
                        (list npc))) *npcs*))
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
(defun display-walk (source npc)
  "Outputs what a player would see given their current location if an
NPC walked from some location to their current location."
  (flet ((get-direction (source destination)
	   (caar (remove-if-not (lambda (x) (eq destination (cadr x)))
				(get-edges source)))))
    (let ((destination (npc-location npc)))
      (if (eq *player-location* source)
	  (unless (eq *player-location* destination)
	    (let ((direction (get-direction source destination)))
	      (princ-stylized-list
	       `(you see ,(npc-name npc) walk to the ,direction))))
	  (when (eq *player-location* destination)
	    (let ((direction (get-direction destination source)))
	      (princ-stylized-list
	       `(,(npc-name npc) enters from the ,direction))))))))


(defgeneric npc-ai (npc motive)
  (:documentation "The AI that controls the passed in NPC each
  turn; Whis is done will do is based on its motive."))
(defgeneric npc-alert (npc location)
  (:documentation "How the AI reacts to being alerted of an event."))

(defun npc-follow-path (npc)
  "Gets an NPC to continue following the path it has."
  (let ((source (npc-location npc)))
    (setf (npc-location npc) (pop (npc-path npc)))
    (display-walk source npc)))

(defmethod npc-ai (npc motive)
  "The basic NPC AI, used when no matching AI is found for the
current motive:
This AI will make the NPC randomly move from one room to another every
once in a while... Or if a path is set, follow it."
  (let ((source (npc-location npc)))
    (if (npc-path npc)
        (npc-follow-path npc)
        (let ((neighbors (mapcar #'cadr (get-edges (npc-location npc)))))
          (when (and (zerop (random 3))
                     (not (zerop (length neighbors))))
            (setf (npc-location npc)
                  (nth (random (length neighbors)) neighbors)))
          (display-walk source npc)))))

(defmethod npc-ai (npc (motive (eql 'find-player)))
  "The NPC motive code for finding the player. (Currently in a dumb
and psychic way)"
  (if (npc-path npc)
      (npc-follow-path npc)
      (setf (npc-path npc) (get-path (npc-location npc) *player-location*)))
  (when (eq (npc-location npc) *player-location*)
    (princ-stylized-list '(there you are!))
    (setf (npc-motive npc) nil)))

(defun update-npcs ()
  "Updates all of the currently active NPCs after they completed their
tasks."
  (flet ((npc-ai-action (npc)
           (npc-ai npc (npc-motive npc))))
    (mapc #'npc-ai-action *npcs*)))

(defun npc-alert-in-range (location distance)
  "Alerts any NPCs within a certain distance of some location of an
event happening"
  (mapc (lambda (npc) (npc-alert npc location))
        (get-npcs-within-range 'hallway distance)))

(defun npc-goto (npc location)
  "Tells an NPC they should go to a certain location."
  (setf (npc-path npc) (get-path (npc-location npc) location)))

(defmethod npc-alert ((npc npc) location)
  "Basic NPC alert AI... When alerted, the NPC goes to investigate the
node the event happened at."
  (npc-goto npc location))

;; Map utility functions
(defun new-location-description (description &optional place)
  "Sets a new description for the passed-in location"
  (setf (car (get-node (or place *player-location*))) description))

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
  (labels ((parse (args acc)
	     (let ((current (car args)))
	       (when current
		 (case current
                   ((in at)
                    (cons acc (cons `(eq ',(cadr args) *player-location*)
                                    (parse (cddr args) acc))))
                   ((has holds)
                    (cons acc (cons `(member ',(cadr args) (inventory))
                                    (parse (cddr args) acc))))
                   ((already)
                    (cons acc (cons `(special-command-run-p ',(cadr args))
                                    (parse (cddr args) acc))))
                   ((see sees)
                    (cons acc (cons `(can-see ',(cadr args) *player-location*)
                                    (parse (cddr args) acc))))
                   (and (cdr (parse (cdr args) '())))
                   (otherwise (parse (cdr args) '())))))))
    (parse (remove-if #'fluff-word-p arg-list) 'and)))

(defun special-command-run-p (command)
  "Checks if a special command has run successfully."
  (find (remove-if #'fluff-word-p command) *events-complete* :test #'equal))

(defun special-command (input)
  "Runs a command configured in the map"
  (let ((event (get-event (car input)))
        (args (cdr input)))
    (let ((form (assoc args event :test #'equal)))
      (if form
          (if (not (special-command-run-p input))
              (if (eval (fourth form))
                  (progn
                    (incf *trouble-points* (second form))
                    (npc-alert-in-range *player-location* (third form))
                    (push input *events-complete*)
                    (when (sixth form)
		      (mapc #'eval (sixth form)))
                    (fifth form))
                  '(you cannot do that.))
              '(you already did that.))
          '(huh?)))))


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
                          (mapcar #'car (get-edges *player-location*))))
          (walk command))
         (t (special-command input)))))))

(defun handle-player (input)
  "Handles the actions a player can take in their given situation."
  (flet ((holding-player (npc)
           (member 'player (npc-holding npc))))
    (let ((holding-npc (car (remove-if-not #'holding-player *npcs*))))
      (if (null holding-npc)
          (princ-stylized-list (game-eval (remove-if #'fluff-word-p input)))
          (if (eq (car input) 'struggle)
              (if (zerop (random 10))
                  (progn
                    (setf (npc-holding holding-npc)
                          (remove 'player (npc-holding holding-npc)))
                    (princ-stylized-list '(you get away!)))
                  (princ-stylized-list
                   '(you struggle... but it is fruitless.)))
              (princ-stylized-list
               `(you "can't" move! ,(npc-name holding-npc) is holding onto
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
