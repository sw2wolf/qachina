;; this needs an installation of lynx, a character based web browser
;; loads very fast, quickly exit with Ctrl-C
;;

(define-macro (help func)
  (if (primitive? (eval func))
    (let (func-name (term func))
      (if (ends-with func-name "?") (replace "?" func-name "p")) 
      (!  (format "lynx /usr/share/doc/newlisp/newlisp_manual.html#%s" func-name)))
    (format "%s is not a built-in function" (term func))))

(define (apropos str (do-print true))
  "Return symbols that matches the regexp."
  (let ((acc (find-all str (symbols) $it
    (lambda (x y) (regex x (name y))))))
    (when (and acc do-print)
      (dolist (item acc)
        (cond
          ((primitive? (eval item))
            (println item "\t" "<primitive>"))
          ((lambda? (eval item))
            (println item "\t" "<lambda>"))
          ((macro? (eval item))
            (println item "\t" "<macro>"))
          ("else"
            (println item)))))
      acc))

(define (utf8?)
  "Non-nil means newLISP is UTF-8 eoncoding are supported."
  (primitive? MAIN:utf8))

(define (newlisp-version)
  "Return newLISP version as integer."
  (sys-info -2))

(define (getpid) (sys-info -3))        ; Return the Process ID of newLISP.
(define (getppid) (sys-info -4))

; iteration
; fast and also returns the whole list
(define (fib n , f)
  (set 'f '(1 0))
  (dotimes (i n)
    (push (+ (f 0) (f 1)) f)))

;;
(constant 'SXF 0.0015) ;手续费
(constant 'YHS 0.001)  ;印花费
(constant 'GHF 1.0)    ;过户费

(define (winG qty pb ps)
"算股票盈利"
  (- (mul qty ps (sub 1 SXF YHS)) (mul 2 GHF) (mul qty pb (add 1 SXF))))

(define (winQ qty pb ps)
"算权证盈利"
  (sub (mul qty ps (sub 1 SXF)) (mul 2 GHF) (mul qty pb (add 1 SXF))))

(define (stopLoss qty pb (lossRate 0.03))
"止损价"
  (let ((tot (mul qty pb (add 1 SXF))))
    (println (format "Stop Loss at:%.2f" (sub pb (div (mul tot lossRate) qty))))
    (println (format "Lost Money:%.2f" (mul tot lossRate)))) 0)

(define (div618 p1 p2)
"黄金分割"
    (let ((ratio '(0. 0.191 0.236 0.382 0.5 0.618 0.809 1.))
          (price (fn (r) (if (<= p1 p2) (add p1 (mul (sub p2 p1) r)) (sub p1 (mul (sub p1 p2) r))))))
          (if (<= p1 p2)
            (dolist (r (reverse ratio)) (println (format "-------%.3f   %.2f-------" r (price r))))
            (dolist (r ratio) (println (format "-------%.3f  %.2f-------" r (price r)))))) 0)

(define (qachina) (spawn 'qachina (exec "cd /media/D/www/qachina; ./start.bat")))

(let ((e (env "NEWLISPDIR")))
  (when (and e (not (directory? e)))
      (println "warning: directory " e " not found.")))

(context MAIN)
