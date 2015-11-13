(defpackage :io.github.thingywhat.prettify
  (:use :common-lisp :asdf)
  (:export :a/an
           :stylize-string
           :stylize-list
           :princ-stylized-list))

(defpackage :io.github.thingywhat.trouble-behind
  (:use :common-lisp :asdf :io.github.thingywhat.prettify))
