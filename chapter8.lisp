(load "graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
; Each street will have a 1-in-15 chance of containing a roadblock
(defparameter *cop-odds* 15)

(defun random-node()
  ; random function returns a random natural number less than *node-num*
  ; 1+ function adds the parameter by 1
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  ; repeat *edge-num* times in loop
  (apply #'append (loop repeat *edge-num*
                        ; collect a edge-pair with every loop
                        collect (edge-pair (random-node) (random-node)))))

; (loop repeat 10
    ; collect 1)
; (1 1 1 1 1 1 1 1 1 1)

;(loop for n from 1 to 10
      ;collect n)

; (1 2 3 4 5 6 7 8 9 10)

(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
                   (eql (car x) node))
                 edge-list))

(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node) 
                       (unless (member node visited)
                         (push node visited)
                         (mapc (lambda (edge)
                                 (traverse (cdr edge)))
                               (direct-edges node edge-list)))))
      (traverse node))
    visited))

(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-islands (nodes)
                          (let* ((connected (get-connected (car nodes) edge-list))
                                 (unconnected (set-difference nodes connected)))
                            (push connected islands)
                            (when unconnected 
                              (find-islands unconnected)))))
      (find-islands nodes))
    islands))

(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num*
                      collect i))
         ; with let* we can reference nodes defined previously 
         (edge-list (connect-all-islands nodes (make-edge-list)))
         (cops (remove-if-not (lambda (x)
                                (zerop (random *cop-odds*)))
                              edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

; generates a alist looks like
; '((1 (2)) (2 (1) (3)) (3 (2)))
(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
            (cons node1
                  (mapcar (lambda (edge)
                            (list (cdr edge)))
                          (remove-duplicates (direct-edges node1 edge-list)
                                             :test #'equal))))
          (remove-duplicates (mapcar #'car edge-list))))

; generates a alist looks like
; ((1 (2)) (2 (1) (3 COPS)) ( 3 (2 COPS)))
(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
            (let ((node1 (car x))
                  (node1-edges (cdr x)))
              (cons node1
                    (mapcar (lambda (edge)
                              (let ((node2 (car edge)))
                                (if (intersection (edge-pair node1 node2)
                                                  edges-with-cops
                                                  :test #'equal)
                                  (list node2 'cops)
                                  edge)))
                            node1-edges))))
          edge-alist))

(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

; check if two nodes are one node paart in the city graph
(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))

(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      ; then check if any of these new nodes are within one
      (some (lambda (x)
              (within-one x b edge-alist))
            ; fist extract all the nodes that are one away
            (neighbors a edge-alist))))

(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num*
                          collect (random-node))))
    (loop for n from 1 to *node-num*
          collect (append (list n)
                          (cond ((eql n wumpus) '(wumpus))
                                ((within-two n wumpus edge-alist) '(blood!)))
                          (cond ((member n glow-worms)
                                 '(glow-worm))
                                ((some (lambda (worm)
                                         (within-one n worm edge-alist))
                                       glow-worms)
                                 '(lights!)))
                          (when (some #'cdr (cdr (assoc n edge-alist)))
                            '(sirens!))))))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city))

(defun find-empty-node()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
      (find-empty-node)
      x)))

(defun draw-city()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))
