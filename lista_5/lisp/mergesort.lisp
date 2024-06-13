(defun merge-lists (left right)
    (cond
        ((null left) right)
        ((null right) left)
        ((<= (car left) (car right))
            (cons (car left) (merge-lists (cdr left) right)))
        (t (cons (car right) (merge-lists left (cdr right))))))

(defun split-sequence (sequence)
    (let* ((half (truncate (/ (length sequence) 2)))
        (first-half (subseq sequence 0 half))
        (second-half (subseq sequence half)))
    (values first-half second-half)))

(defun mergesort (sequence)
    (if (or (null sequence) (null (cdr sequence)))
        sequence
        (multiple-value-bind (first-half second-half) (split-sequence sequence)
            (merge-lists (mergesort first-half) (mergesort second-half)))))

(defun main ()
    (format t "(mergesort (3 1 4 1 5 9 2 6 5 3 5)) = ~a~%" (mergesort '(3 1 4 1 5 9 2 6 5 3 5))))

(main)