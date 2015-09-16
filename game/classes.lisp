(in-package :io.github.thingywhat.trouble-behind)

(defclass actor ()
  ((name
    :documentation "The name of the actor"
    :initarg :name
    :accessor actor-name
    :type string)
   (location
    :documentation "The actor's current location"
    :initarg :location
    :accessor actor-location
    :type symbol)
   (inventory
    :documentation "The name of the symbol that specifies this actor's
    inventory location."
    :initarg :inventory
    :initform (gensym)
    :reader actor-inventory
    :type symbol))
  (:documentation "An actor is anything in the game world that isn't
  an object. This includes the player and NPCs."))

(defclass npc (actor)
  ((path
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
    :documentation "The current motivations of the NPC."
    :initarg :motive
    :initform nil
    :accessor npc-motives
    :type list)
   (seen
    :documentation "A list of noteworthy things this NPC has seen."
    :initarg :seen
    :initform nil
    :accessor npc-seen
    :type list)
   (sounds
    :documentation "An alist of sounds an NPC makes based on their vicinity."
    :initarg :sounds
    :initform '((1 (you hear footsteps.))
                (2 (you hear the faint sound of footsteps.)))
    :accessor npc-sounds
    :type list))
  (:documentation "An NPC is a type of actor that has AI driving
  it. Each active NPC shoukd be added to the *npcs* list below when
  deemed to become \"active\", as that will make the actor
  automagically update according to their AI implementation"))

(defclass player (actor)
  ((trouble-points
    :documentation "How much trouble the player has caused... Also
    doubles as a score."
    :initarg :trouble-points
    :initform 0
    :accessor player-trouble-points
    :type number))
  (:documentation "The player class is a type of actor that is
  controlled by a human."))
