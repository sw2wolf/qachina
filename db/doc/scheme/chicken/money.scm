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
        (format t "Stop Loss at:~$~%" (- pb (/ (* tot lossRate) qty)))
        (format t "Lost Money:~$(~d%)~%" (* tot lossRate) (* 100 lossRate))))

(define (div618 p1 p2)
;"黄金分割"
  (let ((ratio '(0. 0.191 0.236 0.382 0.5 0.618 0.809 1.))
        (price (lambda (r) (if (<= p1 p2) (+ p1 (* (- p2 p1) r)) (- p1 (* (- p1 p2) r))))))
	(if (<= p1 p2)
		(dolist (r (reverse ratio)) (format t "-------~3$   ~$-------~%" r (price r)))
        (dolist (r ratio) (format t "-------~3$  ~$-------~%" r (price r))))))
