(defun read-input (input-file)
  (with-open-file (stream input-file)
    (loop for x = (read stream nil nil)
          while x
          collect x)))

(defstruct node metadata children size)

;; Forward declaration
(declaim (ftype (function (list) t) parse))

(defun parse-children (n body)
  (loop repeat n
        for child = (parse body)
        collect child
        do (setq body (nthcdr (node-size child) body))))

(defun parse (input)
  (destructuring-bind (c m &rest more)
      input
    (if (zerop c)
        (make-node :metadata (subseq more 0 m) :size (+ 2 m))
        (let* ((body (subseq more 0 (- (length more) m)))
               (children (parse-children c body))
               (children-size (reduce #'+ children :key #'node-size))
               (metadata (subseq more children-size (+ children-size m))))
          (make-node :metadata metadata
                     :children children
                     :size (+ 2 m children-size))))))

(defun metadata (tree)
  (nconc (mapcan #'metadata (node-children tree))
         (node-metadata tree)))

(defun day08-part1 (input-file)
  (reduce #'+ (metadata (parse (read-input input-file)))))

(defun value (tree)
  (if (not (node-children tree))
      (reduce #'+ (node-metadata tree))
      (loop for m in (node-metadata tree)
            for idx = (1- m)
            when (< idx (length (node-children tree)))
              sum (value (elt (node-children tree) idx)))))

(defun day08-part2 (input-file)
  (value (parse (read-input input-file))))



