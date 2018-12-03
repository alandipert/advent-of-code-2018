(ql:quickload :cl-ppcre)

(defstruct claim id x y width height)

(defun parse-claim (line)
  (cl-ppcre:register-groups-bind ((#'parse-integer id x y width height))
      ("#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)" line)
    (make-claim :id id :x x :y y :width width :height height)))

(defun make-fabric (width height)
  (make-array (list width height) :initial-element nil))

(defun read-claims (input-file)
  (with-open-file (stream input-file)
    (loop for line = (read-line stream nil nil)
          while line
          collect (parse-claim line))))

(defun mark-claim! (fabric claim)
  (dotimes (x (claim-width claim) fabric)
    (dotimes (y (claim-height claim))
      (pushnew (claim-id claim)
               (aref fabric
                     (+ (claim-x claim) x)
                     (+ (claim-y claim) y))))))

(defun mark-claims! (fabric claims)
  (dolist (claim claims fabric)
    (mark-claim! fabric claim)))

(defun count-claimed (fabric min-claims)
  (destructuring-bind (width height)
      (array-dimensions fabric)
    (let ((claim-count 0))
      (dotimes (x width claim-count)
        (dotimes (y height)
          (if (>= (length (aref fabric x y)) min-claims)
              (incf claim-count)))))))

(defun day03-part1 (input-file)
  (let ((fabric (make-fabric 1000 1000))
        (claims (read-claims input-file)))
    (mark-claims! fabric claims)
    (count-claimed fabric 2)))

(day03-part1 "day03_input.txt")

(defun day03-part2 (input-file)
  (let* ((claims (read-claims input-file))
         (claim-ids (mapcar #'claim-id claims))
         (fabric (mark-claims! (make-fabric 1000 1000) claims)))
    (dotimes (x 1000 claim-ids)
      (dotimes (y 1000)
        (let ((place (aref fabric x y)))
          (if (> (length place) 1)
              (setq claim-ids (nset-difference claim-ids place))))))))

(time (day03-part2 "day03_input.txt"))
