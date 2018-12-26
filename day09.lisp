(ql:quickload :cl-ppcre)

(defun read-input (input-file)
  (cl-ppcre:register-groups-bind
   ((#'parse-integer players marbles))
   ("(\\d+).* (\\d+) points" (with-open-file (stream input-file)
                               (read-line stream nil nil)))
   (list players marbles)))

(defstruct (node (:print-function print-node)) value left right)

(defun print-node (node stream depth)
  (declare (ignore depth))
  (loop for next = node then (node-right next)
        for continue = t then (and next (not (equal next node)))
        while continue
        do (format stream "~A " (node-value next))
        finally (format stream "~%")))

(defun insert (prev-node new-node)
  "Inserts new-node after prev-node and returns new-node."
  (prog1 new-node
    (psetf (node-right new-node) (node-right prev-node)
           (node-left new-node) prev-node
           (node-right prev-node) new-node
           (node-left (node-right prev-node)) new-node)))

(defun unlink (node)
  "Unlinks node and returns the node that was to its right."
  (setf (node-left (node-right node)) (node-left node)
        (node-right (node-left node)) (node-right node)))

(defun move (node distance)
  "If distance is negative, moves left by that amount. Otherwise, moves right."
  (cond ((zerop distance) node)
        ((> distance 0) (move (node-right node) (1- distance)))
        ((< distance 0) (move (node-left node) (1+ distance)))))

(defun make-start ()
  (let ((start (make-node :value 0)))
    (setf (node-right start) start
          (node-left start) start)))

(defun to-zero (node)
  (if (zerop (node-value node))
      node
      (to-zero (node-left node))))

(defun play (players marbles)
  (do ((scores (make-array `(,players) :initial-element 0))
       (node (make-start))
       (m 1 (1+ m))
       (player 0 (mod (1+ player) players)))
      ((>= m marbles) (reduce #'max scores))
    (if (zerop (mod m 23))
        (let ((to-unlink (move node -7)))
          (incf (aref scores player) (+ m (node-value to-unlink)))
          (setq node (unlink to-unlink)))
        (setq node (insert (node-right node) (make-node :value m))))))

(defun day09-part1 (input-file)
  (apply #'play (read-input input-file)))

(defun day09-part2 (input-file)
  (destructuring-bind (players marbles)
      (read-input input-file)
    (play players (* marbles 100))))
