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
  ; appy pretends that the items int he list are separate objects and passes them to the given function as such
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

; (objects-at 'living-room *objects* *object-locations*)
; (whiskey bucket)
(defun objects-at (loc objs obj-locs)
  ; use labels function to define functions locally
  (labels ((at-loc-p (obj) 
                     (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

; (describe-objects 'living-room *objects* *object-locations*)
; (You see a whiskey on the floor. You see a bucket on the floor)
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
                         `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
      ; use progn to wedge in extra commands in a single expression
      ; with progn, only the last evaluation is returned as the value of the full expression
      (progn (setf *location* (car next))
             (look))
      '(you cannot go that way.))))

(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t `(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun say-hello ()
  (princ "Please type your name:")
  (let ((name (read-line)))
    (princ "Nice to meet you, ")
    (princ name)))

;(defun game-repl ()
  ;(loop (print (eval (read)))))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string
               (concatenate 'string "(" (read-line) ")"))))
    ; Local functions can be defined with labels or flet
    ; Since we are not using any recursion in the quote-it function
    ; we can use the simplier flet command
    (flet ((quote-it (x)
                     ; Single quote is just short-hand for a Lisp command called quote
                     ; This means that 'foo and (quote foo) are the same
                     ; We can quote a raw parameter by simply putting the paramter in a list 
                     ; with the quote command in front
                     (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

; One attach method that will break our program is to use reader marcos
; e.g. walk #.{format-harddrive}
; The bottom line is that you can never be sure that a Lisp program using eval or read is completely safe
; When writing production Lisp code, you should try to avoid these two commands when possible.
(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
    (eval sexp)
    '(i do not konw that command)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ; Turn on the cap parameter for the rest of the string
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ; We've encountered a quotation mark
            ; As long as the lit value is set, the tweak-text function prevents the capitalization rules
            ; from being reached
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil list)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

; (game-print '(not only does this sentence have a "comma," it also mentions the "iPad."))
; Not only does this sentence have a comma, it also mentions the iPad.
(defun game-print (lst)
  ; coerce function converts the string to a list of characters
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  ; prin1-to-string converts the symbol list into a sting
                                                  ; The to-string part means this function doesn't dump the result
                                                  ; to the screen, but just returns it as a string
                                                  ; The 1 means that it will stay on a single line.
                                                  (prin1-to-string lst))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))
