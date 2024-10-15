(defun add-numbers-in-list (num-list)
  "Takes a list of numbers and return the sum.
Argument NUM-LIST list of numbers."
  (apply `+ num-list))

(defun sieve (n)
  "Return a list of all prime numbers in a list using the Sieve of Eratosthenes.
Argument N max of list."
  (let ((primes (make-vector (+ n 1) t))
        (result '()))
    (setf (aref primes 0) nil)
    (setf (aref primes 1) nil)
    (dotimes (i (ceiling (sqrt n)))
      (when (aref primes i)
        (dotimes (j (+ (* i i) i) n i)
          (setf (aref primes j) nil))))
    (dotimes (i (length primes))
      (when (and (aref primes i) (<= i n))
        (push i result)))
    (reverse result)))

(provide 'sum-of-primes)
