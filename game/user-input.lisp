(in-package :io.github.thingywhat.trouble-behind)

(defun fluff-word-p (word)
  "Returns if the passed in word is a fluff word and should be
ignored."
  (member word '(the at to my a can is out with through)))
