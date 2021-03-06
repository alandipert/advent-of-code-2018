(defun reaction-position (string)
  (loop for i from 0 to (- (length string) 2)
        for x = (elt string i)
        for y = (elt string (1+ i))
        if (and (char-equal x y)
                (or (and (lower-case-p x)
                         (upper-case-p y))
                    (and (upper-case-p x)
                         (lower-case-p y))))
          do (return i)))

(defun remove-substring (string start &optional (end (1+ start)))
  (concatenate 'string
               (subseq string 0 start)
               (subseq string end (length string))))

(defun react (string)
  (loop for pos = (reaction-position string)
        while (and pos (> (length string) 1))
        do (setq string (remove-substring string pos (+ pos 2)))
        finally (return string)))

(defun day05-part1 (input-file)
  (with-open-file (stream input-file)
    (react (read-line stream nil nil))))
