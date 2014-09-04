(defparameter *primes* (map-into 
    (make-array 100 :element-type 'fixnum :adjustable t :fill-pointer 0)
    #'identity '(2 3)))

(declaim (vector *primes*)
	 (optimize speed (safety 0))
	 (inline next-prime prime))

(define-symbol-macro last-prime (elt *primes* (- (length *primes*) 1)))

(defun next-prime ()
  (vector-push-extend
      (loop
        for i = (+ 2 last-prime) then (+ i 2)
        for r = (floor (sqrt i))
        when (loop for p across *primes*
                while (<= p r)
                    never (zerop (mod i p)))
            do (return i))
      *primes*))

(defun prime (n)
  (elt *primes* n))

(defun nth-prime (n)
  (loop repeat (max 0 (1+ (- n (length *primes*))))
       do (next-prime))
  (prime n))

(defun ensure-primes-to (n)
  (loop while (> n last-prime)
     do (next-prime)))

(defun prime-index (n &aux (l (length *primes*)))
  (loop 
     for i = (floor (/ l 2)) then 
       (min (- l 1) (if (> n (prime i)) (+ i d) 
			(if (< n (prime i))
			    (- i d)
			    (return i))))
     for d = (ceiling (/ i 2)) then (let ((d2 (ceiling (/ d 2))))
				      (max 0 (if (= d2 d) (- d 1) d2)))
     when (or (zerop d) (= d (prime i))) return i))


(defun not-primep (n &aux (r (floor (sqrt n))))
  (loop for i = 0 then (1+ i)
     for p = (nth-prime i)
     while (<= p r)
     when (zerop (mod n p)) return p))

(defun primep (n)
  (not (not-primep n)))

(defun prime-factors (n)
  (when (> n 1)
    (let ((p (not-primep n)))
      (if p
	  (cons p (prime-factors (/ n p)))
	  (list n)))))

(defun gcf (arg &rest args)
  (let ((fs (prime-factors arg)))
    (apply #'*
	   (loop for f in fs
	      when (loop for a in args always (zerop (mod a f)))
	      collect f))))

(length *primes*)
