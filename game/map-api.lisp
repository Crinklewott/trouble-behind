(in-package :io.github.thingywhat.trouble-behind)

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
