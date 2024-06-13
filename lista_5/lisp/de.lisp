(defun extended-gcd (a b)
    (if (= b 0)
        (values 1 0 a)
        (multiple-value-bind (x1 y1 d) (extended-gcd b (mod a b))
            (values y1 (- x1 (* (floor a b) y1)) d))))

(defun de (a b)
    (multiple-value-bind (x0 y0 d) (extended-gcd a b)
        (when (plusp d)
            (setq x0 (* x0 (floor (/ (abs d) (gcd a b)))))
            (setq y0 (* y0 (floor (/ (abs d) (gcd a b))))))
        (list x0 y0 d)))

(defun main ()
    (format t "(de 10 15) = ~a~%" (de 10 15))
    (format t "(de 39 17) = ~a~%" (de 39 17))
    (format t "(de 441 35) = ~a~%" (de 441 35)))

(main)
