#lang racket
(for/sum ([i (in-range 3000000)]) (random))

(require mzlib/os)
(with-output-to-file your-pid-file (lambda () (write (getpid))))

(define roman-table
  (hash #\M 1000
        #\D 500
        #\C 100
        #\L 50
        #\X 10
        #\V 5
        #\I 1))

(define immutable-inner #hash((x . 0)(y . 0)))
(define inner (make-hash '((x . 0)(y . 0))))

(define hex '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F))

(regexp-split #rx" " (string-downcase "hellow world ! racket is funning "))

(define (bounding-box vecs)
  (for/fold ([xmin (vector-ref (car vecs) 0)]
             [xmax (vector-ref (car vecs) 0)]
             [ymin (vector-ref (car vecs) 1)]
             [ymax (vector-ref (car vecs) 1)])
      ([p (in-list vecs)])
      (let ([x (vector-ref p 0)]
            [y (vector-ref p 1)])
        (values (min x xmin)
                (max x xmax)
                (min y ymin)
                (max y ymax)))))

(for/and ([line (in-lines ip)]
          [count (in-naturals)])
         (match line
		   [(regexp #px"=> \"PRIVMSG #emacs :(.*)\"$" (list _ stuff))
            (when (and (not (regexp-match #px"^\\w+:" stuff))
                       (not (regexp-match #px"Arooooooooooo" stuff)))
			  (hash-table-increment! counts-by-quote stuff))]
              [_ #f])
         count)

(let ([N 6])
     (define results (make-vector (add1 N) 0))
     (for ([i 100000])
       (define s 0)
       (define fs (for/list ([i N]) (future (λ () (set! s (add1 s))))))
       (for-each touch fs)
       (vector-set! results s (add1 (vector-ref results s))))
     results)
'#(0 0 3 0 2 452 99543)

(with-handlers ([exn?
                 (lambda (e)
				   (notifier "error, module not reloaded (~a)"
							 (exn-message e))
                   (notifier "~a~%" 
							 (continuation-mark-set->context (exn-continuation-marks e))))])
  (namespace-require '(only scheme module #%top-interaction))
  (load/use-compiled path))

