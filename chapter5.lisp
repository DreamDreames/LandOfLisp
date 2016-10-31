(defparameter *nodes* '((living-room (you are in the living-room.
                                          a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                                     there is a well in fornt of you.))
                        (attic (you are in the attic.
                                    there is a giant welding torch in the corner.))))

; (describe-location 'living-room *nodes*)
; (you are in the living-room. a wizard is snoring loudly on the couch.)
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

; (describe-path '(garden west door))
; (there is a door going west from here.)
(defun describe-path (edge)
  ; quasiquoting, allows us to create chunks of data that have small pieces of Lisp code embedded in them
  ; backquote [`] switches from code to data mode
  ; a backquote can also be unquoted using the comma character, to filp back into code mode
  ; after all, a comma does look just like an upside-down backquote.
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))


; (describe-paths 'living-room *edges*)
; (there is a door going west from here. there is a laddor going upstairs from here.)
(defun describe-paths (location edges)
  ; mapcar takes another function and a list, and then applies this function to every member of a list
  ; the #' symbol is a shorthand for the function operator
  ; (mapcar #'car '((foo bar) (baz qux))) will be converted to (mapcar (function car) '((foo bar) (baz qux)))
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))
