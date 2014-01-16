;(if #f #f) is the simplest way to get the special "unspecified" value, the same one that is returned by many other functions like 'for-each' that don't have anything to return.
(define (print x) (if (not (eq? x (if #f #f))) (write x)))

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
  (let ((ratio '(0.0 0.191 0.236 0.382 0.5 0.618 0.809 1.0))
        (price (lambda (r) (if (<= p1 p2) (+ p1 (* (- p2 p1) r)) (- p1 (* (- p1 p2) r))))))
	(if (<= p1 p2)
		(for-each (lambda(r) (format #t "---~3$  ~$---~%" r (price r))) (reverse ratio))
        (for-each (lambda(r) (format #t "---~3$  ~$---~%" r (price r))) ratio))))

(define +ssq-hit-num+ "/media/D/qachina/db/doc/money/ssqHitNum.txt")
(define +ssq-num+ "/media/D/qachina/db/doc/money/ssqNum.txt")

;; (define *RED-NUMS* (cdr (iota 34)))
;; (define *BLUE-NUMS* (cdr (iota 17)))

;; (define (win-ssq count noRed noBlue)
;;   (let ((yesRed (set-difference *RED-NUMS* (str2lst noRed)))
;; 		(okBlue (pickNums count (set-difference *BLUE-NUMS* (str2lst noBlue))))
;; 		(num '()))
;; 	(call-with-output-file +ssq-num+
;; 	  (lambda(h)
;; 		(map (lambda(n)
;; 		   (set! num (lst2str (append (pickNums 6 yesRed) (list (list-ref okBlue n)))))
;; 			   (display num) (newline)
;; 			   (display num h) (newline h)
;; 			 ) (iota count))
;; 		))) #t)

;; (define (hit-ssq term hitNum)
;;   (let ((hitNumLst (str2lst hitNum)) (hitR 0) (hitB 0) (num '()) (hitH 0))
;; 	(set! hitH (open-file +ssq-hit-num+ "a"))
;; 	(display (string-append term " " hitNum "\n") hitH)
;; 	(close-port hitH)
;; 	(call-with-input-file +ssq-num+
;; 	  (lambda (h)
;; 		(do ((line (read-line h) (read-line h))) ((eof-object? line))
;; 		  (set! num (str2lst line))
;; 		  (set! hitR (length (intersection hitNumLst (list-head num 6))))
;; 		  (if (= (list-ref hitNumLst 6) (list-ref num 6))
;; 			  (set! hitB 1)
;; 			  (set! hitB 0))
;; 		  (format #t "~s   (~d ~d)   ~s~%" line hitR hitB (hitDesc hitR hitB)))))))

;; (define (pickNums count from)
;;   (let ((res 0))
;; 	(sort (map (lambda (c)
;; 		   (set! res (list-ref from (random (length from))))
;; 		   (set! from (delete res from))
;; 		   res) (iota count)) <)))

;; (define (str2lst str)
;;   (map (lambda (c) (string->number c)) (string-split str #\space)))

;; (define (lst2str lst)
;;   (let ((res ""))
;; 	(map (lambda(n)
;; 		   (if (string-null? res)
;; 			   (set! res (number->string n))
;; 			   (set! res (string-append res " " (number->string n)))))
;; 		   lst) res))

;; (define (hitDesc red blue)
;;     (let ((res "X"))
;;     (cond
;;         ((and (= red 6) (= blue 1)) (set! res "First"))
;;         ((and (= red 6) (= blue 0)) (set! res "Second"))
;;         ((and (= red 5) (= blue 1)) (set! res "Third(3000)"))
;;         ((or (and (= red 5) (= blue 0)) (and (= red 4) (= blue 1))) (set! res "Fourth(200)"))
;;         ((or (and (= red 4) (= blue 0)) (and (= red 3) (= blue 1))) (set! res "Fifth(10)"))
;;         ((= blue 1) (set! res "Sixth(5)")))
;;     res))

(define (his) (system (string-append "tail " +ssq-hit-num+)))

(define (sd word)
  (system (string-append "sdcv -n " word)))

;; (define (qachina)
;;   (make-thread (system "cd /media/D/qachina; ./start.bat")))

(define (sh cmd) (system cmd))
