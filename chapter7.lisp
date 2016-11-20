; Common Lisp will use more complex printing routines for printing data structures.
; These routines will check to see if you've run into a previously seen cons cell
(setf *print-circle* t)
(defparameter foo '(1 2 3))
; Circular List
(setf (cdddr foo) foo)
(print foo)

; Association Lists or alist for short
; An alist consists of key/value pairs stored in a list
(defparameter *drink-order* '((bill . double-espresso)
                              (lisa . small-drip-coffee)
                              (john . medium-latte)))

(assoc 'lisa *drink-order*)
; (LISA . SMALL-DRIP-COFFEE)

(push '(lisa . large-mocha-with-whipped-cream) *drink-order*)
; ((LISA . LARGE-MOCHA-WITH-WHIPPED-CREAM)
;  (BILL . DOUBLE-ESPRESSO)
;  (LISA . SMALL-DRIP-COFFEE)
;  (JOHN . MEDIUM-LATTE))

(assoc 'lisa *drink-order*)
; (LISA . LARGE-MOCHA-WITH-WHIPPED-CREAM)

; Tree-like data
(defparameter *house* '((walls (mortar (cement)
                                       (water)
                                       (sand))
                               (bricks))
                        (windows (glass)
                                 (frame)
                                 (curtains))
                        (roof (shingles)
                              (chimney))))

(defparameter *wizard-nodes* '((living-room (you are in the living-room.
                                                 a wizard is snoring loudly on the couch.))
                               (garden (you are in a beautiful garden.
                                            there is a well in front of you.))
                               (attic (you are in the attic. 
                                           there is a giant welding torch in the corner.))))

(defparameter *wizard-edges* '((living-room (garden west door)
                                            (attic upstairs ladder))
                               (garden (living-room east door))
                               (attic (living-room downstairs ladder))))

(defun dot-name (exp)
  ; We substitute only those characters that aren't alphanumeric
  ; The substitute-if function substitutes values based on the result of a test function
  ; Lisp already has a predicate function that tells us if a character is alphanumeric, called alphanumericp
  ; complement function create the opposite of alphanumeric
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defparameter *max-label-length* 30)
(defun dot-label (exp)
  (if exp
    ; write-to-string function writes an expression to a string
    ; The :pretty parameter is an example of a keyword parameter
    ; which is used to let you choose which parameters you want to pass in
    ; In the case of write-to-string (set :pretty to nil), it tells Lisp not to alter the string to make it pretty
    (let ((s (write-to-string exp :pretty nil)))
      (if (> (length s) *max-label-length*)
        (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
        s))
    ""))

(defun nodes->dot (nodes)
  ; mapc is a slightly more efficient variant of mapcar; 
  ; the difference is that it does not return the transformed list
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

(nodes->dot *wizard-nodes*)
