;; lib_lisp.lisp

;; (1) binomial

(defun binomial (n k)
    (cond
        ((= k 0) 1)
        ((= n 0) 0)
        (t (/ (* n (binomial (1- n) (1- k))) k))))

;; (2) binomial2

(defun binomial2 (n k)
    (let ((triangle (make-array (1+ n) :initial-element nil)))
        (dotimes (i (1+ n))
            (setf (aref triangle i) (make-array (1+ i) :initial-element 1)))
        (dotimes (i n)
            (dotimes (j i)
                (setf (aref (aref triangle (1+ i)) (1+ j))
                    (+ (aref (aref triangle i) j) (aref (aref triangle i) (1+ j))))))
        (aref (aref triangle n) k)))

;; (3) mergesort

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

;; (4) de

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

;; (5) prime_factors

(defun prime_factors (n &optional (d 2))
    (if (<= n 1)
        '()
        (if (= 0 (mod n d))
            (cons d (prime_factors (/ n d) d))
            (prime_factors n (1+ d)))))

;; (6) totient

(defun number-sequence (start end)
    (if (> start end)
        nil
        (cons start (number-sequence (1+ start) end))))

(defun totient (n)
    (length (remove-if-not (lambda (x) (= (gcd x n) 1)) (number-sequence 1 n))))

;; (7) totient2

(defun totient2 (n)
    (let* ((factors (remove-duplicates (prime_factors n)))
        (result n))
    (dolist (p factors result)
        (setf result (* result (- 1 (/ 1.0 p)))))
    (floor result)))

;; (8) primes

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
    (format t "(binomial 5 2) = ~a~%" (binomial 5 2))
    (format t "(binomial 40 15) = ~a~%" (binomial 40 15))
    (format t "(binomial 250 35) = ~a~%" (binomial 6 4))

    (format t "(binomial2 5 2) = ~a~%" (binomial2 5 2))
    (format t "(binomial2 40 15) = ~a~%" (binomial2 40 15))
    (format t "(binomial2 250 35) = ~a~%" (binomial2 6 4))

    (format t "(mergesort (3 1 4 2)) = ~a~%" (mergesort '(3 1 4 2)))
    (format t "(mergesort (6 3 1 8 5 7 4 2)) = ~a~%" (mergesort '(6 3 1 8 5 7 4 2)))
    (format t "(mergesort (2 4 1 3 3 4 2 1)) = ~a~%" (mergesort '(2 4 1 3 3 4 2 1)))

    (format t "(de 10 15) = ~a~%" (de 10 15))
    (format t "(de 39 17) = ~a~%" (de 39 17))
    (format t "(de 441 35) = ~a~%" (de 441 35))

    (format t "(prime_factors 21) = ~a~%" (prime_factors 21))
    (format t "(prime_factors 100) = ~a~%" (prime_factors 100))
    (format t "(prime_factors 42900) = ~a~%" (prime_factors 42900))

    (format t "totient(10) = ~a~%" (totient 10))
    (format t "totient(50) = ~a~%" (totient 50))
    (format t "totient(100) = ~a~%" (totient 100))

    (format t "totient2(10) = ~a~%" (totient2 10))
    (format t "totient2(50) = ~a~%" (totient2 50))
    (format t "totient2(100) = ~a~%" (totient2 100))


    (format t "primes(10) = ~a~%" (primes 10))
    (format t "primes(50) = ~a~%" (primes 50))
    (format t "primes(100) = ~a~%" (primes 100)))

(main)