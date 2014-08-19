;(if #f #f) is the simplest way to get the special "unspecified" value, the same one that is returned by many other functions like 'for-each' that don't have anything to return.
(define (print x) (if (not (eq? x (if #f #f))) (write x)))

(define (string-split str ch)
  (let ((len (string-length str)))
    (letrec
      ((split
        (lambda (a b)
          (cond
            ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
              ((char=? ch (string-ref str b)) (if (= a b)
                (split (+ 1 a) (+ 1 b))
                  (cons (substring str a b) (split b b))))
                (else (split a (+ 1 b)))))))
                  (split 0 0))))

(define intersection
    (lambda (set1 . sets)
      (cond
        [(null? sets) set1]
        [(any empty? sets) (empty-set)]
        [else (choose
                (lambda (elt)
                  (every (lambda (set) (member? elt set)) sets)) set1)])))

(define member? memq)
(define empty? null?)

(define any
    (lambda (pred? ls)
      (let loop ([ls ls])
        (cond
          [(null? ls) #f]
          [(pred? (car ls)) #t]
          [else (loop (cdr ls))]))))

(define choose
    (lambda (pred? ls)
      (fold (lambda (elt tail)
              (if (pred? elt)
                  (cons elt tail)
                  tail))
            '()
            ls)))

(define fold
    (lambda (op base ls)
      (let recur ([ls ls])
        (if (null? ls)
            base
            (op (car ls) (recur (cdr ls)))))))

(define every
    (lambda (pred? ls)
      (let loop ([ls ls])
        (cond
          [(null? ls) #t]
          [(pred? (car ls)) (loop (cdr ls))]
          [else #f]))))

(define reverse-filter
    (lambda (pred? ls)
      (fold (lambda (elt tail)
              (if (pred? elt)
                  tail
                  (cons elt tail)))
            '()
            ls)))

(define difference
    (lambda (set1 . sets)
      (let ((sets (reverse-filter empty? sets)))
        (cond
          [(null? sets) set1]
          [else (reverse-filter (lambda (elt)
                                  (any (lambda (set) 
                                         (member? elt set)) 
                                       sets))
                                set1)]))))

(define (his) (system (string-append "tail " +ssq-hit-num+)))

(define (sd word)
  (system (string-append "sdcv -n " word)))

;; (define (qachina)
;;   (make-thread (system "cd /media/D/qachina; ./start.bat")))

(define (sh cmd) (system cmd))

(define (fib n)
   (fib-iter 1 0 n))

(define (fib-iter a b count)
   (if (= count 0)
       b
       (fib-iter (+ a b) a (- count 1))))

(define (fac n)
   (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
   (if (> counter max-count)
       product
       (fact-iter (* counter product)
                  (+ counter 1)
                  max-count)))
;
;investment
;
(define SXF 0.0015) ;手续费
(define YHS 0.001)  ;印花费
(define GHF 1.0)    ;过户费

(define (winG qty pb ps)
;"算股票盈利"
  (- (* qty ps (- 1 SXF YHS)) (* 2 GHF) (* qty pb (+ 1 SXF))))

(define (winQ qty pb ps)
;"算权证盈利"
  (- (* qty ps (- 1 SXF)) (* 2 GHF) (* qty pb (+ 1 SXF))))

(define (stopLoss qty pb lossRate)
;"止损价"
  (let ((tot (* qty pb (+ 1 SXF))))
        (format #t "Stop Loss at:~$~%" (- pb (/ (* tot lossRate) qty)))
        (format #t "Lost Money:~$(~d%)~%" (* tot lossRate) (* 100 lossRate))))

(define (div618 p1 p2)
;"黄金分割"
  (let ((ratio '(0. 0.191 0.236 0.382 0.5 0.618 0.809 1.))
        (price (lambda (r) (if (<= p1 p2) (+ p1 (* (- p2 p1) r)) (- p1 (* (- p1 p2) r))))))
	(if (<= p1 p2)
		(for-each (lambda(r) (format #t "---~3$  ~$---~%" r (price r))) (reverse ratio))
        (for-each (lambda(r) (format #t "---~3$  ~$---~%" r (price r))) ratio))))

;
; lottery
;
(define +ssq-hit-num+ "/media/D/qachina/db/doc/money/ssqHitNum.txt")
(define +ssq-num+ "/media/D/qachina/db/doc/money/ssqNum.txt")

(define *RED-NUMS* (cdr (iota 34)))
(define *BLUE-NUMS* (cdr (iota 17)))

(define (win-ssq count noRed noBlue)
  (let ((yesRed (difference *RED-NUMS* (str2lst noRed)))
		(okBlue (pickNums count (difference *BLUE-NUMS* (str2lst noBlue))))
		(num '()))
	;(seed->random-state (current-time))
	(call-with-output-file +ssq-num+
	  (lambda(h)
		(map (lambda(n)
		   (set! num (lst2str (append (pickNums 6 yesRed) (list (list-ref okBlue n)))))
			   (display num) (newline)
			   (display num h) (newline h)
			 ) (iota count))
		) 'replace)) #t)

(define (hit-ssq term hitNum)
  (let ((hitNumLst (str2lst hitNum)) (hitR 0) (hitB 0) (num '()) (hitH 0))
	(set! hitH (open-file +ssq-hit-num+ "a"))
	(display (string-append term " " hitNum "\n") hitH)
	(close-port hitH)
	(call-with-input-file +ssq-num+
	  (lambda (h)
		(do ((line (get-line h) (get-line h))) ((eof-object? line))
		  (set! num (str2lst line))
		  (set! hitR (length (intersection
							    (list-head hitNumLst 6) (list-head num 6))))
		  (if (= (list-ref hitNumLst 6) (list-ref num 6))
			  (set! hitB 1)
			  (set! hitB 0))
		  (format #t "~s   (~d ~d)   ~s~%" line hitR hitB (hitDesc hitR hitB)))))))

;; (define (good-red)
;;   (let ((tab (make-hash-table 33)) (nums '()))
;; 	(for-each (lambda (i) ((put-hash-table! tab i 0)) *RED-NUMS*)
;;     (call-with-input-file +ssq-hit-num+
;; 	  (lambda (port)
;; 		(do ((line (read-line port) (read-line port))) ((eof-object? line))
;; 		  (set! nums (butlast (str2lst (substring line 6)) 1))
;; 		  (for-each (lambda (n)
;; 					  (put-hash-table! tab n (1+ (get-hash-table tab n 0)))) nums)
;; 		)))

;; 	(sort
;; 	 (map (lambda (e) (car e))
;; 		 (list-head (sort (hash-map->list cons tab)
;; 						  (lambda (left right)
;; 							(> (cdr left) (cdr right)))) 21)) <))))

(define (hitDesc red blue)
    (let ((res "X"))
    (cond
        ((and (= red 6) (= blue 1)) (set! res "First"))
        ((and (= red 6) (= blue 0)) (set! res "Second"))
        ((and (= red 5) (= blue 1)) (set! res "Third(3000)"))
        ((or (and (= red 5) (= blue 0)) (and (= red 4) (= blue 1))) (set! res "Fourth(200)"))
        ((or (and (= red 4) (= blue 0)) (and (= red 3) (= blue 1))) (set! res "Fifth(10)"))
        ((= blue 1) (set! res "Sixth(5)")))
    res))

(define (pickNums count from)
  (let ((res 0))
	(sort < (map (lambda(c)
		   (set! res (list-ref from (random (length from))))
		   (set! from (remove res from))
		   res) (iota count)))))

(define (str2lst str)
  (map (lambda(c) (string->number c)) (string-split str #\space)))

(define (lst2str lst)
  (let ((res ""))
	(map (lambda(n)
		   (if (string=? res "")
			   (set! res (number->string n))
			   (set! res (string-append res " " (number->string n)))))
		   lst) res))

