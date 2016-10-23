(defun guess-my-number ()
  ; arithmetic shift
  (ash (+ *small* *big*) -1))

(defun smaller()
  ; subtracts 1 from the result
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun bigger()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

(defun start-over()
  (defparameter *small* 1)
  (defparameter *big* 100)
  (guess-my-number))

