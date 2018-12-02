(defun freqs (str)
  (loop with freqs = (make-hash-table)
        for c across str
        do (incf (gethash c freqs 0))
        finally (return freqs)))

(defun invert-freqs (freqs)
  (loop with counts = (make-hash-table)
        for k being the hash-keys in freqs using (hash-value v)
        do (pushnew k (gethash v counts))
        finally (return counts)))

(defun day02-part1 (input-file)
  (with-open-file (stream input-file)
    (loop for id = (read stream nil nil)
          with two = 0
          with three = 0
          while id
          do (let ((counts (invert-freqs (freqs (symbol-name id)))))
               (if (gethash 2 counts) (incf two))
               (if (gethash 3 counts) (incf three)))
          finally (return (* two three)))))

(day02-part1 "day02_input.txt")

(defun read-ids (input-file)
  (with-open-file (stream input-file)
    (loop for id = (read stream nil nil)
          while id
          collect (symbol-name id))))

(defun difference (id1 id2)
  (assert (eql (length id1) (length id2)))
  (loop with diff = 0
        with idxs = ()
        for i from 0 to (1- (length id1))
        if (not (eql (elt id1 i) (elt id2 i)))
          do (progn
               (push i idxs)
               (incf diff))
        finally (return (values diff idxs))))

(defun idxs-removed (str idxs)
  (loop for c across str
        for i from 0
        if (not (member i idxs))
          collect c into ret
        finally (return (coerce ret 'string))))

(defun day02-part2 (input-file)
  (let ((boxes
          (let ((ids (read-ids input-file)))
            (loop named outer
                  for id1 in ids do
                    (loop for id2 in ids do
                      (if (eql 1 (difference id1 id2))
                          (return-from outer (list id1 id2))))))))
    (multiple-value-bind (diff idxs) (apply #'difference boxes)
      (string-downcase (idxs-removed (car boxes) idxs)))))
