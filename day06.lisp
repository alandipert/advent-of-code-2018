(ql:quickload :cl-ppcre)

(defun read-input (input-file)
  (with-open-file (stream input-file)
    (loop for line = (read-line stream nil nil)
          while line
          collect
          (cl-ppcre:register-groups-bind ((#'parse-integer x y))
              ("(\\d+), (\\d+)" line)
            (cons x y))
            into points
          finally (return (apply #'vector points)))))

;; (setq input (read-input "day06_input_small.txt"))

(defun find-biggest-point (points)
  (let ((biggest-x 0)
        (biggest-y 0))
    (loop for (x . y) across points
          do (progn
               (if (> x biggest-x)
                   (setq biggest-x x))
               (if (> y biggest-y)
                   (setq biggest-y y)))
          finally (return (cons biggest-x biggest-y)))))

(defun make-grid (points)
  (let* ((biggest-point (find-biggest-point points)))
    (make-array (list (1+ (car biggest-point))
                      (1+ (cdr biggest-point))))))

(defun distance (p1 p2)
  (+ (abs (- (car p1) (car p2)))
     (abs (- (cdr p1) (cdr p2)))))

(defun distances-to-points (from-point to-points)
  (loop for to-point across to-points
        for i from 0
        collect (cons i (distance from-point to-point))))

(defun closest-to (from-point to-points)
  (let* ((distnces (distances-to-points from-point to-points))
         (sorted (sort distances #'< :key #'cdr))
         (ties (remove-if-not (lambda (point)
                                (= (cdr point) (cdar sorted)))
                              sorted)))
    (when (= 1 (length ties))
      (caar ties))))

(defun closeness-grid (points)
  (let* ((grid (make-grid points)))
    (destructuring-bind (width height)
        (array-dimensions grid)
      (loop for x below width
            finally (return grid)
            do (loop for y below height
                     do (setf (aref grid x y)
                              (closest-to (cons x y) points)))))))

(defun find-infinite (grid)
  (destructuring-bind (width height)
      (array-dimensions grid)
    (remove nil
            (remove-duplicates
             (concatenate 'list
                          (loop for x below width
                                nconc
                                (list (aref grid x 0)
                                      (aref grid x (1- height))))
                          (loop for y below height
                                nconc
                                (list (aref grid 0 y)
                                      (aref grid (1- width) y))))))))

(defun remove-infinite! (grid)
  (destructuring-bind (width height)
      (array-dimensions grid)
    (let ((infinites (find-infinite  grid)))
      (loop for x below width
            do (loop for y below height
                     if (member (aref grid x y) infinites)
                       do (setf (aref grid x y) nil))))))

(defun count-remaining (grid)
  (destructuring-bind (width height)
      (array-dimensions grid)
    (let ((counts (make-hash-table)))
      (loop for x below width
            finally (return counts)
            do (loop for y below height
                     for id = (aref grid x y)
                     when id
                     do (incf (gethash id counts 0)))))))

(defun hash-table->alist (ht)
  (loop for k being the hash-keys of ht using (hash-value v)
        collect (cons k v)))

(defun day06-part1 (input-file)
  (let* ((points (read-input input-file))
         (grid (closeness-grid points)))
    (remove-infinite! grid)
    (sort (hash-table->alist (count-remaining grid))
          #'>
          :key #'cdr)))

(defun day06-part2 (input-file &optional (max 10000))
  (let* ((points (read-input input-file))
         (biggest (find-biggest-point points)))
    (loop with region-size = 0
          finally (return region-size)
          for x to (car biggest)
          do (loop for y to (cdr biggest)
                   for sum = (reduce #'+
                                     (distances-to-points
                                      (cons x y)
                                      points)
                                     :key #'cdr)
                   if (< sum max)
                     do (incf region-size)))))
