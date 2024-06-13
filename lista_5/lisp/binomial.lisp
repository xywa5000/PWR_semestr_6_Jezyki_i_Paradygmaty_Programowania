(defun binomial (n k)
    (cond
        ((= k 0) 1)
        ((= n 0) 0)
        (t (/ (* n (binomial (1- n) (1- k))) k))))

(defun main ()
    (format t "(binomial 5 2) = ~a~%" (binomial 5 2))
    (format t "(binomial 40 15) = ~a~%" (binomial 40 15))
    (format t "(binomial 250 35) = ~a~%" (binomial 6 4)))

(main)