;; (ql-dist:install-dist "http://beta.quicklisp.org/dist/quicklisp/2018-10-18/distinfo.txt"
;;                       :replace t
;;                       :prompt nil)

(ql:quickload :cl-ppcre)
(ql:quickload :local-time)

(defstruct entry timestamp activity id)

(defun get-activity (activity)
  (cond ((cl-ppcre:scan "wakes up" activity) :wake)
        ((cl-ppcre:scan "falls asleep" activity) :sleep)
        (t (cl-ppcre:register-groups-bind ((#'parse-integer id))
               ("Guard #(\\d+) begins shift" activity)
             (values :begin id)))))

(defun parse-entry (line)
  (cl-ppcre:register-groups-bind ((#'parse-integer year month day hour minute) activity)
      ("\\[(\\d+)-(\\d+)-(\\d+) (\\d+):(\\d+)\\] (.*)" line)
    (let* ((ts (local-time:encode-timestamp 0 0 minute hour day month year))
           (entry (make-entry :timestamp ts)))
        (multiple-value-bind (kw id?) (get-activity activity)
          (setf (entry-activity entry) kw
                (entry-id entry) id?)
          entry))))

(defun get-sorted-entries (input-file)
  (with-open-file (stream input-file)
    (loop for line = (read-line stream nil nil)
          while line
          collect (parse-entry line) into entries
          finally (return (sort entries #'local-time:timestamp< :key #'entry-timestamp)))))

(defun guard-ids (entries)
  (remove-duplicates (remove nil (mapcar #'entry-id entries))))

(defun guard-entries (entries guard-id &aux during-shift)
  (loop for entry in entries
        if (eq (entry-activity entry) :begin)
          do (setq during-shift (= (entry-id entry) guard-id))
        if during-shift collect entry))

(defun sleep-intervals (guard-entries &aux last-asleep)
  (loop for entry in guard-entries
        if (eq (entry-activity entry) :sleep)
          do (setq last-asleep (entry-timestamp entry))
        else
          if (eq (entry-activity entry) :wake)
            collect (cons last-asleep (entry-timestamp entry))))

(defun time-asleep (sleep-intervals)
  (loop for (from . to) in sleep-intervals
        sum (- (local-time:timestamp-minute to)
               (local-time:timestamp-minute from))))

(defun minutes-in-sleep-interval (interval)
  (destructuring-bind (from . to) interval
    (loop for minute from (local-time:timestamp-minute from)
            below (local-time:timestamp-minute to)
          collect minute)))

(defun frequencies (xs)
  (let ((freqs (make-hash-table)))
    (dolist (x xs freqs)
      (incf (gethash x freqs 0)))))

(defun minutes-asleep (sleep-intervals)
  (frequencies (mapcan #'minutes-in-sleep-interval sleep-intervals)))

(defun minute-most-asleep (minutes-asleep)
  (let ((most-asleep (cons nil -1)))
    (loop for minute being the hash-keys of minutes-asleep using (hash-value count)
          if (> count (cdr most-asleep))
            do (setf (car most-asleep) minute
                     (cdr most-asleep) count)
          finally (return (car most-asleep)))))

(defun guard-asleep-for (entries guard-id)
  (time-asleep (sleep-intervals (guard-entries entries guard-id))))

(defun guard-asleep-minute (entries guard-id)
  (minute-most-asleep (minutes-asleep (sleep-intervals (guard-entries entries guard-id)))))

(defun day04-part1 (input-file)
  (let* ((entries (get-sorted-entries input-file))
         (guard-ids (guard-ids entries))
         (results (loop for guard-id in guard-ids
                        collect (list
                                 guard-id
                                 (guard-asleep-for entries guard-id)
                                 (guard-asleep-minute entries guard-id)))))
    (destructuring-bind (id asleep-for minute)
        (car (sort results #'> :key #'second))
      (declare (ignore asleep-for))
      (* id minute))))

(defun day-frequencies (entries)
  (let ((guard-ids (guard-ids entries))
        (minutes (make-hash-table)))
    (loop for minute from 0 to 59
          do (setf (gethash minute minutes) (make-hash-table)))
    (loop for minute from 0 to 59 do
      (loop for guard-id in guard-ids
            for times-asleep = (length
                                (remove-if (lambda (m) (not (= m minute)))
                                           (mapcan #'minutes-in-sleep-interval
                                                   (sleep-intervals
                                                    (guard-entries entries guard-id)))))
            do (setf (gethash guard-id (gethash minute minutes)) times-asleep)))
    minutes))

(defun hash-table->alist (ht)
  (loop for k being the hash-keys of ht using (hash-value v)
        collect (cons k v)))

(defun most-minutes-asleep (minute-freqs)
  (car (sort (hash-table->alist minute-freqs) #'> :key #'cdr)))

(defun day04-part2 (input-file)
  (let* ((entries (get-sorted-entries input-file))
         (day-frequencies (day-frequencies entries))
         (minute-winners (loop for minute from 0 to 59
                               collect (list minute (most-minutes-asleep
                                                     (gethash minute day-frequencies))))))
    (destructuring-bind (minute (guard-id . times))
        (car (sort minute-winners #'> :key #'cdadr))
      (declare (ignore times))
      (* guard-id minute))))
