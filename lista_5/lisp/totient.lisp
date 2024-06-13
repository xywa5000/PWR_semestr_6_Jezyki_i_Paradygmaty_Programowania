(defun number-sequence (start end)
    (if (> start end)
        nil
        (cons start (number-sequence (1+ start) end))))

(defun totient (n)
    (length (remove-if-not (lambda (x) (= (gcd x n) 1)) (number-sequence 1 n))))

(defun main ()
    (format t "totient(10) = ~a~%" (totient 10))
    (format t "totient(20) = ~a~%" (totient 20))
    (format t "totient(50) = ~a~%" (totient 50))
    (format t "totient(100) = ~a~%" (totient 100)))

(main)