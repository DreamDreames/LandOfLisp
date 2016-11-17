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
