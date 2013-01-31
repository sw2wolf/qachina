(do ((x 0 (+ x 1)))
    ((= x 11))
  (display (fact x))
  (newline))

(define passwd (open-input-file "/etc/passwd"))

(let lp ((ch (read-char passwd)))
  (if (not (eof-object? ch))
    (lp (read-char passwd))))

(define (read-sign port)
  (let loop ((c (peek-char port)) (s ""))
    (case c
      ((#\+ #\-)
       (let ((ch (read-char port)))
         (string-append s (string ch))))
      (else s))))

(define (json-build-string scm port escape)
  (simple-format
   port "\"~A\""
   (list->string
    (fold-right append '()
                (map
                 (lambda (c)
                   (case c
                     ((#\" #\\) `(#\\ ,c))
                     ((#\bs) '(#\\ #\b))
                     ((#\ff) '(#\\ #\f))
                     ((#\lf) '(#\\ #\n))
                     ((#\cr) '(#\\ #\r))
                     ((#\ht) '(#\\ #\t))
                     ((#\/) (if escape `(#\\ ,c) (list c)))
                     (else (string->list (build-char-string c)))))
                 (string->list scm))))))
(define (unicode->string unicode)
  (format #f "\\u~4,'0x" unicode))

(define (char->unicode-string c)
  (let ((unicode (char->integer c)))
    (if (< unicode 32)
        (unicode->string unicode)
        (string c))))

(define (u8v-2->unicode bv)
  (let ((bv0 (bytevector-u8-ref bv 0))
        (bv1 (bytevector-u8-ref bv 1)))
    (+ (ash (logand bv0 #b00011111) 6)
       (logand bv1 #b00111111))))

(define (u8v-3->unicode bv)
  (let ((bv0 (bytevector-u8-ref bv 0))
        (bv1 (bytevector-u8-ref bv 1))
        (bv2 (bytevector-u8-ref bv 2)))
    (+ (ash (logand bv0 #b00001111) 12)
       (ash (logand bv1 #b00111111) 6)
       (logand bv2 #b00111111))))

(define (build-char-string c)
  (let* ((bv (string->utf8 (string c)))
         (len (bytevector-length bv)))
    (cond
     ;; A single byte UTF-8
     ((eq? len 1) (char->unicode-string c))
     ;; If we have a 2 or 3 byte UTF-8 we need to output it as \uHHHH
     ((or (eq? len 2) (eq? len 3))
      (let ((unicode (if (eq? len 2)
                         (u8v-2->unicode bv)
                         (u8v-3->unicode bv))))
        (unicode->string unicode)))
     ;; Anything else should wrong, hopefully.
     (else (throw 'json-invalid)))))

(define-syntax json-read-delimited
  (syntax-rules ()
    ((json-read-delimited port delim read-func)
     (let loop ((c (read-char port)))
       (case c
         ;; skip whitespace
         ((#\ht #\vt #\lf #\cr #\sp) (loop (peek-char port)))
         ;; read contents
         ((delim) (read-func port))
         (else (throw 'json-invalid)))))))

(define (pickNums count from)
   (define len (length from))
   (define (decorate from)
	 (map (lambda (x) (cons x (random len))) from))
   (define (<? x y) (< (cdr x) (cdr y)))
   (map car (list-head (sort (decorate from) <?) count)))

(define (random-subset n lst)
  (let ((v (make-vector n)))
    (do ((i 0 (+ i 1))
         (lst lst (cdr lst)))
        ((null? lst))
      (let ((k (random (+ i 1))))
        (if (< k n)
            (begin (if (< i n)
                       (vector-set! v i (vector-ref v k)))
                   (vector-set! v k (car lst))))))
    (vector->list v)))

(define (random-subset n lst)
  (let ((v (make-vector n)))
    (for-each (lambda (i elt)
                (let ((k (random (+ i 1))))
                  (when (< k n)
                    (when (< i n)
                      (vector-set! v i (vector-ref v k)))
                    (vector-set! v k elt))))
              (iota (length lst))
              lst)
    (vector->list v)))



(define (main . args)
  (let* ((ll ((@ (srfi srfi-1) iota) (read) 1))
		 (len (length ll))
		 (m (1- (/ len 2))))
    (let lp((a (list-head ll (1+ m)))
			(b (list-tail ll (1+ m))) (n 1))
      (and (< n len)
		   (for-each
			(lambda (x y) (display x)(display " ")(display y)(display " ")) a b)
		   (newline)
           (lp (append (list 1 (car b)) (cdr a))
			   (append (cdr b) (list (list-ref a m)))
			   (1+ n))))))

(open-input-file "Person.proto")
(call-with-output-file "Person.scm"
  (lambda (port)
    (pretty-print proto port)))

(use-modules (rnrs))
 
(define (binary->string bin #:key (base 16) (endiannes 'little))
  (let ((bl (bytevector->uint-list bin endiannes 1))
	(n (car (assoc-ref '((2 "~b") (8 "~o") (16 "~x")) base))))
    (call-with-output-string
     (lambda (port)
       (for-each (lambda (i) (format port n i)) bl)))))

(use-modules (rnrs))
 
(define (string->binary str #:key (base 16) (endiannes 'little))
  (let ((n (string->number str base))
	(f (case endiannes 
	     ((big) identity) 
	     ((little) reverse) 
	     (else (error "wrong endiannes" endiannes)))))
    (let lp ((n n) (result '()))
      (if (zero? n) 
	  (list->u8vector (f result))
          (lp (ash n -8) (cons (logand n #xff) result))))))
(define bbb '())
(set! bbb (append bbb `(,(length aaa))))

(define a2 #(1 2 3 4 5))
(vector-set! a2 3 100)

(define (call-with-input-file filename proc)
     (let ((port (open-input-file filename)))
       (with-exception-handler
         (lambda (x)
           (close-input-port port)
           x)
         (lambda ()
           (call-with-values (lambda () (proc port))
             (lambda vals
               (close-input-port port)
               (apply values vals)))))))

(define (file-size filename)
  (call-with-input-file filename (lambda (port)
    (let loop ((c (read-char port))
               (count 0))
      (if (eof-object? c) 
          count
          (loop (read-char port) (+ 1 count)))))))

(define (flatten x)
    (cond ((null? x) '())
          ((not (pair? x)) (list x))
          (else (append (flatten (car x))
                        (flatten (cdr x))))))


(use-modules (oop goops))
 1. 重载 + 函数, 以使字符串可以相加, 结果为字符串的顺序组合:
guile> (define-method (+ (x <string>) (y <string>))
...        (string-append x y))
guile> (+ "abc" "def")
"abcdef"
guile> (+ "abc" "123" "def")
"abc123def"

(define-class <2d-vector> ()
        (x #:init-value 0 #:accessor x #:init-keyword #:x)
        (y #:init-value 0 #:accessor y #:init-keyword #:y))
这样我们就定义了一个数学中二维向量, 其中有两个属性: x 和 y.

3. 对象生成
可以用 make 来创建一个二维向量:
guile> (define v1 (make <2d-vector>))               ; x, y 被设为默认值 (0, 0)
guile> (define v2 (make <2d-vector> #:x 1 #:y 2))   ; x, y 被设为 (1, 2)
guile> v1
#<<2d-vector> b75fe9e0>
guile> v2
#<<2d-vector> b75fde80>

4. 对象属性
 可以访问和改变对象的 x 和 y 的值:
guile> (x v1)
0
guile> (y v1)
0
guile> (x v2)
1
guile> (y v2)
2
guile> (set! (x v1) 3)
guile> (set! (y v1) 4)
guile> (x v1)
3
guile> (y v1)
4
5. 重载函数
 可以重载 +, 以使向量相加:
guile> (define-method (+ (v1 <2d-vector>) (v2 <2d-vector>))
...        (make <2d-vector>
...              #:x (+ (x v1) (x v2))
...              #:y (+ (y v1) (y v2))))

guile> (define v3 (+ v1 v2))
guile> (x v3)
4
guile> (y v3)
6
6. 判断类型
 可以得到一个对象的类型, 或者判断一个对象是否属于某个类型:
guile> (class-of v1)
#<<class> <2d-vector> b75fdc30>
guile> (class-of 1)
#<<class> <integer> b760ec30>
guile> (is-a? v1 <2d-vector>)
#t
