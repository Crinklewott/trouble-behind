(in-package :io.github.thingywhat.trouble-behind)

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
