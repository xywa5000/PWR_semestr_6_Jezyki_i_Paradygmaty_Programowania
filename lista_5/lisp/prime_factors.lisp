(defun prime_factors (n &optional (d 2))
    (if (<= n 1)
        '()
        (if (= 0 (mod n d))
            (cons d (prime_factors (/ n d) d))
            (prime_factors n (1+ d)))))

(defun main ()
    (format t "(prime_factors 21) = ~a~%" (prime_factors 21))
    (format t "(prime_factors 100) = ~a~%" (prime_factors 100))
    (format t "(prime_factors 42900) = ~a~%" (prime_factors 42900)))

(main)