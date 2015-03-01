;; State
(defparameter *map*
  (with-open-file (nodes "map.lmap" :direction :input)
    (read nodes))
  "The map that contains all of the location nodes as well as the
  edges that connect them.")

(defparameter *player-location*
  (cdr (assoc 'player-location *map*))
  "The current location of the player")

(defparameter *item-locations* 
  (mapcar (lambda (x) (cons (car x) (caadr x)))
	  (cadr (assoc 'item-location-details *map*)))
  "An alist containing the locations of items")

(defparameter *events-complete* '()
  "A list of events that have been successfully completed.")

;; Dynamically loads things from the map
(defmacro defmap (entry getter-name fetcher-name)
  "Defines a get-all-<things> function to fetch the alist for some
specific thing from the loaded map and get-<things> to fetch an
individual item"
  `(let ((,entry (cadr (assoc ',entry *map*))))
     (defun ,fetcher-name (key)
       (cdr (assoc key ,entry)))
     (defun ,getter-name ()
       ,entry)))

;; Load each type of map item we care about and creates a whole mess
;; of utility functions
(defmap nodes get-nodes get-node)
(defmap edges get-all-edges get-edges)
(defmap items get-items get-item)
(defmap item-location-details get-all-item-details get-item-details)
(defmap events get-event-list get-event)

;; Grammar and string output functions
(defun fluff-word-p (word)
  "Returns if the passed in word is a fluff word and should be
ignored."
  (member word '(the at towards my a)))

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
  (labels ((style (char-list caps)
	     (when char-list
	       (let ((first (car char-list))
		     (rest (cdr char-list)))
		 (cond
		   ((char= first #\space) (cons #\space (style rest caps)))
		   ((char= first #\.) (cons #\. (style rest t)))
		   (caps (cons (char-upcase first) (style rest nil)))
		   (t (cons (char-downcase first) (style rest nil))))))))
    (coerce (style (coerce (string-trim "()" str) 'list) t) 'string)))

(defun stylize-list (list)
  "Stylizes a list as a pretty string."
  (stylize-string (prin1-to-string list)))

;; Querying
(defun item-location (item)
  "Fetches the location of an item"
  (cdr (assoc item *item-locations*)))

(defun item-moved (item)
  "Returns t if an item has moved from its original location, nil
otherwise."
  (loop for i in *item-locations*
     when (eq item (car i)) collect i into acc
     when (< 1 (length acc)) return t))

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
		 (if (item-moved item)
		     `(there is ,(a/an item) ,item on the floor here.)
		     (cadadr item-detail))))))
	  item-details))

(defun can-see (item location)
  "Checks if you can see an item currently"
  (let ((loc (item-location item)))
    (or (eq location loc)
	(eq 'inventory loc))))

;; Player location-oriented functions
(defun look ()
  "Outputs what the player sees around them."
  (append
   (car (get-node *player-location*))
   (describe-edges (get-edges *player-location*))
   (describe-items-at-location (get-all-item-details) *player-location*)))

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
	  (progn (push (cons item 'inventory) *item-locations*)
	    `(you pick up the ,item)))
      `(you cannot see ,(a/an item) ,item from here.)))

(defun drop (item)
  "Lets the player drop an item at their current location"
  (if (member item (inventory))
      (progn (push (cons item *player-location*) *item-locations*)
	     `(you drop the ,item on the floor.))
      `(you dont have that.)))

;; Game REPL functions
(defun tb-eval (&rest input)
  "Evaluates user input from (read) in a controlled manner and
performs the respective game commands passed in."
  (let* ((input (car input))
	 (command (car input)))
    (cond ((member command '(inventory i items))
	   (or (inventory) '(nothing)))
	  ((member command '(look inspect describe l))
	   (if (cdr input)
	       (look-at (cadr input))
	       (look)))
	  ((member command '(get pickup))
	   (if (cdr input)
	       (pickup (cadr input))
	       '(pick up what?)))
	  ((member command '(drop leave))
	   (if (cdr input)
	       (drop (cadr input))
	       '(drop what?)))
	  ((member command '(walk go run))
	   (if (cdr input)
	       (walk (cadr input))
	       '(walk where?)))
	  ((member command '(n e s w))
	   (tb-eval (cond ((eq command 'n) '(north))
			  ((eq command 'e) '(east))
			  ((eq command 's) '(south))
			  ((eq command 'w) '(west)))))
	  ((member command
		   (append '(north east south west)
			   (mapcar #'car (get-edges *player-location*))))
	   (walk command))
	  (t (special-command input)))))

(defun tb-loop ()
  "Loops through user input passing it to tb-eval and stylyzing the
output."
  (loop for input = '(look)
     then (read-from-string (concatenate 'string "(" (read-line) ")"))
     when (eq (car input) 'quit)
     return t
     do (progn
	  (princ (stylize-list (tb-eval (remove-if #'fluff-word-p input))))
	  (fresh-line))))

;; Advaced metaprogramming thingies
(defmacro when-player (&rest arg-list)
  "Checks if the player meets certain conditions in plain english. For example:
(when-player has blanket)
...is valid, and will check if the player has a blanket in their
inventory...

You can chain together statements with \"and\" as well, so the
following works too:
(when-player has blanket and is in your-bedroom)

Any words the macro doesn't understand are simply ignored.

Valid words are:
- in, at <place>
- has, holds <item>
- see, sees <thing>
- already <special command>"
  (labels ((parse (args acc)
	     (let ((current (car args)))
	       (when current
		 (cond ((member current '(in at))
			(cons acc (cons `(eq ',(cadr args) *player-location*)
					(parse (cddr args) acc))))
		       ((member current '(has holds))
			(cons acc (cons `(member ',(cadr args) (inventory))
					(parse (cddr args) acc))))
		       ((member current '(already))
			(cons acc (cons `(special-command-run-p ',(cadr args))
					(parse (cddr args) acc))))
		       ((member current '(see sees))
			(cons acc (cons `(can-see ',(cadr args) *player-location*)
					(parse (cddr args) acc))))
		       ((eq current 'and)
			(cdr (parse (cdr args) '())))
		       (t (parse (cdr args) '())))))))
    (parse arg-list 'and)))

(defun special-command (input)
  "Runs a command configured in the map"
  (let ((event (get-event (car input)))
        (args (cdr input)))
    (let ((form (assoc args event :test #'equal)))
      (if (eval (cadr form))
          (progn (eval (cadddr form))
                 (push input *events-complete*)
                 (caddr form))
          '(you cannot do that.)))))

(defun special-command-run-p (command)
  "Checks if a special command has run successfully."
  (find (remove-if #'fluff-word-p command) *events-complete* :test #'equal))
