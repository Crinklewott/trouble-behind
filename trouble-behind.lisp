;; State
(defparameter *player-location* 'your-bedroom
  "The current location of the player")

(defparameter *map*
  (with-open-file (nodes "map.lmap" :direction :input)
    (read nodes))
  "The map that contains all of the location nodes as well as the
  edges that connect them.")

(defparameter *item-locations* 
  (mapcar (lambda (x) (cons (car x) (caadr x)))
	  (cadr (assoc 'item-location-details *map*)))
  "An alist containing the locations of items")

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

;; Grammar and string output functions
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
	(direction (cadr edge)))
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

(defun look-at (item location)
  "Gets the player to look at an item if they can see it."
  (if (can-see item location)
      (describe-item (get-item item))
      `(you cannot see any ,item around here.)))

;; Player location-oriented functions
(defun look ()
  "Outputs what the player sees around them."
  (append
   (car (get-node *player-location*))
   (describe-edges (get-edges *player-location*))
   (describe-items-at-location (get-all-item-details) *player-location*)))
