(use-modules (ice-9 common-list))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 threads))
(use-modules (ice-9 format))
(use-modules (ice-9 pretty-print))

;(if #f #f) is the simplest way to get the special "unspecified" value, the same one that is returned by many other functions like 'for-each' that don't have anything to return.
(define pp pretty-print)

(define (sd word)
  (system (string-append "sdcv -n " word)))

(define (qachina)
  (make-thread (system "cd /media/D/qachina; ./start.bat")))

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

(define (print x) (if (not (eq? x (if #f #f))) (write x)))

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
  (let ((yesRed (set-difference *RED-NUMS* (str2lst noRed)))
		(okBlue (pickNums count (set-difference *BLUE-NUMS* (str2lst noBlue))))
		(num '()))
	(seed->random-state (current-time))
	(call-with-output-file +ssq-num+
	  (lambda(h)
		(map (lambda(n)
		   (set! num (lst2str (append (pickNums 6 yesRed) (list (list-ref okBlue n)))))
			   (display num) (newline)
			   (display num h) (newline h)
			 ) (iota count))
		))) #t)

(define (hitnum-saved? term)
   (call/cc
	(lambda (k)
	  (call-with-input-file +ssq-hit-num+
		(lambda (port)
		  (do ((line (read-line port) (read-line port))) ((eof-object? line))
			(if (string=? term (string-take line 5)) (k #t)))
		  )) #f)))

;(define port (open path (logior O_WRONLY O_APPEND O_CREAT)))

(define (hit-ssq term hitNum)
  (let ((hitNumLst (str2lst hitNum)) (hitR 0) (hitB 0) (num '()) (hitH 0))
	(if (not (hitnum-saved? term))
		(begin
		  (set! hitH (open-file +ssq-hit-num+ "a"))
		  (display (string-append term " " hitNum "\n") hitH)
		  (close-port hitH)))

	(format #t "Good red hit: ~d~%"
			(length (intersection (list-head hitNumLst 6) (good-red))))

	(call-with-input-file +ssq-num+
	  (lambda (h)
		(do ((line (read-line h) (read-line h))) ((eof-object? line))
		  (set! num (str2lst line))
		  (set! hitR (length (intersection 
							    (list-head hitNumLst 6) (list-head num 6))))
		  (if (= (list-ref hitNumLst 6) (list-ref num 6))
			  (set! hitB 1)
			  (set! hitB 0))
		  (format #t "~s   (~d ~d)   ~s~%" line hitR hitB (hitDesc hitR hitB)))))))

(define (good-red)
  (let ((tab (make-hash-table 33)) (nums '()))
	(for-each (lambda (i) (hash-set! tab i 0)) *RED-NUMS*)
    (call-with-input-file +ssq-hit-num+
	  (lambda (port)
		(do ((line (read-line port) (read-line port))) ((eof-object? line))
		  (set! nums (butlast (str2lst (substring line 6)) 1))
		  (for-each (lambda (n)
					  (hash-set! tab n (1+ (hash-ref tab n)))) nums)
		)))

	(sort
	 (map (lambda (e) (car e))
		 (list-head (sort (hash-map->list cons tab)
						  (lambda (left right)
							(> (cdr left) (cdr right)))) 21)) <)))

(define (pickNums count from)
  (let ((res 0))
	(sort
	 (map (lambda (c)
			(set! res (list-ref from (random (length from))))
			(set! from (delete res from))
			(usleep 500000)
			res) (iota count)) <)))

(define (str2lst str)
  (map (lambda(c) (string->number c)) (string-tokenize str)))

(define (lst2str lst)
  (let ((res ""))
	(map (lambda(n)
		   (if (string-null? res)
			   (set! res (number->string n))
			   (set! res (string-append res " " (number->string n)))))
		   lst) res))

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

(define (his) (system (string-append "tail " +ssq-hit-num+)))

