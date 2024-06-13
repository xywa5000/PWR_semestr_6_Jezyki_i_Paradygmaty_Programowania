(defun sieve (lst)
    (if (null lst)
        nil
        (let ((p (car lst)))
            (cons p (sieve (remove-if (lambda (x) (zerop (mod x p))) (cdr lst)))))))

(defun primes (n)
    (if (< n 2)
        nil
        (sieve (loop for i from 2 to n collect i))))

(defun main ()
    (format t "primes(10) = ~a~%" (primes 10))
    (format t "primes(50) = ~a~%" (primes 50))
    (format t "primes(100) = ~a~%" (primes 100)))

(main)