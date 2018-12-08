(ql:quickload :cl-ppcre)

(defun read-input (input-file)
  (with-open-file (stream input-file)
    (loop for line = (read-line stream nil nil)
          while line
          collect
          (cl-ppcre:register-groups-bind ((#'intern from to))
              ("Step (\\w) .* (\\w) can begin." line)
            (cons from to)))))

(defun requires (pairs step-name)
  (loop for (from . to) in pairs
        if (eq to step-name)
          collect from))

(defun sort-symbols (symbols)
  (sort symbols #'string< :key #'symbol-name))

(defun available (completed pairs &aux available)
  (loop for (from . to) in pairs
        if (subsetp (requires pairs to) completed)
          do (pushnew to available)
        else
          if (not (find from pairs :key #'cdr))
            do (pushnew from available)
        finally (return
                  (sort-symbols
                   (set-difference available completed)))))

(defun day07-part1 (input-file &aux completed)
  (let ((pairs (read-input input-file)))
    (loop for (next) = (available completed pairs)
          while next
          collect (symbol-name next) into strings
          do (push next completed)
          finally (return (apply #'concatenate 'string strings)))))

(defun step-time (step)
  (+ 60 (- (char-code (coerce (symbol-name step) 'character)) 65)))

(defun gather-completed! (workers)
  (loop for i below (length workers)
        for (step . n) = (aref workers i)
        when (and n (zerop n))
          collect step
          and do (setf (aref workers i) nil)))

(defun start-work! (workers available)
  (loop for i below (length workers)
        for worker = (aref workers i)
        if (and available (not worker))
          do (let ((step (pop available)))
               (setf (aref workers i)
                     (cons step (step-time step))))
        finally (return workers)))

(defun do-work! (workers)
  (loop for i below (length workers)
        for worker = (aref workers i)
        if worker do (decf (cdr (aref workers i)))
          finally (return workers)))

(defun in-progress (workers)
  (map 'list #'car (remove nil workers)))

(defun day07-part2 (input-file &key (num-workers 5))
  (do* ((pairs (read-input input-file))
        (completed ()
                   (nconc completed (gather-completed! workers)))
        (available (available nil pairs)
                   (nset-difference
                    (available completed pairs)
                    (in-progress workers)))
        (workers (start-work!
                  (make-array `(,num-workers) :initial-element nil)
                  available)
                 (start-work! (do-work! workers) available))
        (time 0 (1+ time)))
       ((and (every #'null workers)
             (not available))
        time)
    (format t "~A ~A~%" time workers)))
