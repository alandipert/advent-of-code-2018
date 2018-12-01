(defun day01-part1 (input-file)
  (with-open-file (stream input-file)
    (loop for num = (read stream nil nil)
          while num
          sum num)))

(day01-part1 "day01_input.txt")

(defun day01-part2 (input-file)
  (let ((changes (with-open-file (stream input-file)
                   (loop for num = (read stream nil nil)
                         while num
                         collect num)))
        (frequency 0)
        (frequencies-reached (make-hash-table)))
    (setf (gethash frequency frequencies-reached) t)
    (loop named outer do
      (loop for change in changes
            do (if (gethash (incf frequency change) frequencies-reached)
                   (return-from outer frequency)
                   (setf (gethash frequency frequencies-reached) t))))))

(day01-part2 "day01_input.txt")
