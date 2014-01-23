
;;;;;;
(catch #t (lambda () (/ 1 0)) (lambda (key . args) (display "ehhhhh")))

(define libc-obj (dynamic-link "libc.so"))
libc-obj
⇒ #<dynamic-object "libc.so">
(dynamic-args-call 'rand libc-obj '())
⇒ 269167349
(dynamic-unlink libc-obj)
libc-obj
⇒ #<dynamic-object "libc.so" (unlinked)>

;;;;;;
;guile
;autogen == autoreconf -i --force --verbose
;./configure --prefix=/home/sw2wolf/guile/ BDW_GC_LIBS="-L/usr/local/lib -lgc-threaded"

;;;;;;
#!/usr/local/bin/petite --script 

(for-each 
 (lambda (x) (display x) (newline)) 
 (cdr (command-line)))

;;;;;;
(let* ((yin ((lambda (foo) (newline) foo)
             (call/cc (lambda (bar) bar))))
       (yang ((lambda (foo) (write-char #\*) foo)
              (call/cc (lambda (bar) bar)))))
  (yin yang))

;;;;;;
(define-module (guile-wm wm)
  #:use-module (xlib xlib))

(define-once wm-display #f)
(define-once wm-display-string (or (getenv "DISPLAY") ":0"))
(define-once wm-event-hook (make-hook 1))

(define rc-file-location 
  (string-append (passwd:dir (getpw (getuid))) "/.guilewmrc"))

(define-public (wm-init!)
  "Connect to a running X server and begin listening for events"
  (set! wm-display (x-open-display! wm-display-string))
  (let ((wm-root (x-root-window wm-display)))
    (x-select-input! wm-root (logior ButtonPressMask ExposureMask KeyPressMask))
    (wm-event-hook-refresh)
    (if (file-exists? rc-file-location) (load rc-file-location))
    (dynamic-wind
      (lambda () (x-flush! wm-display))
      (lambda () (x-event-loop! wm-display wm-event-hook))
      (lambda () (x-close-display! wm-display)))))

(define-public (wm-event-hook-refresh)
  "Refresh the event hook with the hooks listed in wm-event-hooks"
  (reset-hook! wm-event-hook)
  (for-each (lambda (hook) (add-hook! wm-event-hook hook)) wm-event-hooks))

(define-public (wm-shell-command command)
  "Execute COMMAND in a shell"
  (if (= (primitive-fork) 0)
      (let ((env (cons 
                  (format #f "DISPLAY=~a.~a" wm-display-string 
                          (x-screen-number-of-screen (x-screen-of-display 
wm-display))) 
                  (environ))))
        (execle "/bin/sh" env "/bin/sh" "-c" command))))

;; guile-xlib doesn't have support for keysyms yet, so I just use raw
;; keycodes here
(define default-key-map
  `((24 . ,(lambda (event) (x-event-loop-quit! (x-event:button event))))
    (26 ,wm-shell-command "emacs")
    (28 ,wm-shell-command "xterm")))

(define (mapped-key-handler map)
  "Return a key handler that maps keycodes to commands"
  (lambda (event)
    (if (= (x-event:type event) KeyPress) 
        (let ((command (assq-ref map (x-event:keycode event))))
          (if command
            (if (list? command) 
                (apply (car command) (cdr command))
                (command event)))))))

(define-public wm-event-hooks (list (mapped-key-handler default-key-map)))

;You also need a startup script like this:
(use-modules (guile-wm wm))
(wm-init!)

;; The fun part is that you can put a line like
;; guile --listen=37147 -L /path/to/module/.../ \
;;   /path/to/startup/script/wm.scm               

;; in your .xinitrc or xsession file, and then you can connect to that
;; listening process from Geiser or something like that and hack the wm
;; while it's running.

;---------------------------
(case (rc-method rc)
  ((GET) (uri-query (request-uri (rc-req rc))))
  ((POST) ((@ (rnrs) utf8->string) (rc-body rc)))
  (else (throw 'artanis-err 405 "wrong method for query!" (rc-method rc))))

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
;1. 重载 + 函数, 以使字符串可以相加, 结果为字符串的顺序组合:
guile> (define-method (+ (x <string>) (y <string>))
...        (string-append x y))
guile> (+ "abc" "def")
"abcdef"
guile> (+ "abc" "123" "def")
"abc123def"

(define-class <2d-vector> ()
        (x #:init-value 0 #:accessor x #:init-keyword #:x)
        (y #:init-value 0 #:accessor y #:init-keyword #:y))
;这样我们就定义了一个数学中二维向量, 其中有两个属性: x 和 y.

;3. 对象生成
;可以用 make 来创建一个二维向量:
guile> (define v1 (make <2d-vector>))               ; x, y 被设为默认值 (0, 0)
guile> (define v2 (make <2d-vector> #:x 1 #:y 2))   ; x, y 被设为 (1, 2)
guile> v1
#<<2d-vector> b75fe9e0>
guile> v2
#<<2d-vector> b75fde80>

;4. 对象属性
;可以访问和改变对象的 x 和 y 的值:
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

;5. 重载函数
;可以重载 +, 以使向量相加:
guile> (define-method (+ (v1 <2d-vector>) (v2 <2d-vector>))
...        (make <2d-vector>
...              #:x (+ (x v1) (x v2))
...              #:y (+ (y v1) (y v2))))

guile> (define v3 (+ v1 v2))
guile> (x v3)
4
guile> (y v3)
6

;6. 判断类型
;可以得到一个对象的类型, 或者判断一个对象是否属于某个类型:
guile> (class-of v1)
#<<class> <2d-vector> b75fdc30>

guile> (class-of 1)
#<<class> <integer> b760ec30>
guile> (is-a? v1 <2d-vector>)
#t

;;;;;;
;; Guile Common Problems

;;  guile 的配置文件是什么？
;;  ~/.guile

;;  guile 说在 load path 里找不到文件怎么办？
;;  guile 的 load path 是由 %load-path 变量控制的。把路径加入这个变量就行了。

;;  或者设置环境变量 GUILE_LOAD_PATH.

;;  怎样知道现在系统里加入了哪些 feature?
;;  *features* 变量。

;;  guile 能使用 syntax-case 系统吗？
;;  能。它在 guile 的 ice-9 模块里。
;; (use-syntax (ice-9 syncase))
;;  之后就能使用 syntax-case 了。

;;  在 Guile 怎样调试 Scheme 程序？
;;  使用 (ice-9 debug) 模块就行了。(debug) 就可以进入 debugger.

;;  可以使用 trace untrace trace-stack untrace-stack 来跟踪函数。

;;  如果想要更方便的设置断点，可以使用 guile-debugger。可以在这里下载:
;; http://www.ossau.uklinux.net/guile/index.html
;;  在程序里使用
;; (use-modules (ossau breakpoints))
;;  就能设置断点了。

;;  guile 提供怎样的帮助系统？
;;  (help)
;;  通用的帮助系统。
;;  (apropos "thread")
;;  显示当前系统里含有 "thread" 字样的函数 或者变量，和它们的位置。
;;  (source apropos)
;;  显示 apropos 函数的源代码。

;;  怎样使用 Guile 的面向对象系统 GOOPS?
;;  在 Guile 里使用 goops 模块就行了。
;; (use-modules (oop goops))

;;  比如，我们来重载一个加法操作符 "+":
;; (define-method (+ (x <string>) (y <string>))
;;   (string-append x y))
;; (+ 1 2)
;; (+ "abc" "de")

;;  怎样统计函数运行的时间？
;; (ice-9 time)
;; (use-modules (ice-9 time))
;; (time (sleep 3))

;;  怎样让 Guile 具有 expect 的功能？
;; (use-modules (ice-9 expect))

;;  怎样方便的显示信息，就跟 C 的 printf 那样？
;;  使用 format 就行了。先加载 format 模块。
;; (use-modules (ice-9 format))

;;  然后使用 format
;; format destination format-string args ...
;;  destination 是输出的端口。如果是一个 port 就把格式化好的内容 送到那个端口。如果是 #t 就输出到 current output port。如果是 就输出到一个 string。如果是一个数就输出到 stderr.

;;  格式化串里常用字符的含义：
;;  ~~
;;  一个 ~ 符号。
;;  ~%
;;  换行符。
;;  ~t
;;  一个 TAB.
;;  ~a
;;  像 display 那样输出 arg。
;;  ~s
;;  像 write 那样输出 arg。
;;  ~y
;;  漂亮的打印 arg.
;;  ~d
;;  decimal 整数。
;;  ~x
;;  hexadecimal 整数。
;;  ~o
;;  octal 整数。
;;  ~b
;;  binary 整数。
;;  ~r
;;  数字读音输出。比如 10 被输出为 "ten"。如果使用 ~:r 就会输 出序数词。如果使用 ~@r 就会输出大写罗马数字。~:@r 输出罗 马序数词。
;;  ~f
;;  浮点数。比如 1.23
;;  ~e
;;  以科学计数法输出浮点数。比如 1.34E+0.
;;  ~g
;;  选择 ~f 和 ~e 中较短的一个作为输出。
;;  ~$
;;  只输出两位小数。适合用于输出钱的数目。看看它的名字就知 道。
;;  ~c
;;  输出 char.
;;  ~? ~k
;;  把 arg 作为一个格式串。接下来的参数必须是一个 list 包含格式串里的 value.
;;  ~!
;;  flush.
;;  ~#\newline
;;  续行
;;  ~(
;;  大小写转换开始。如果是 ~( 就是全部小写。如果是 ~@(，转 换成 capitcalize，如果是 ~:@( 转换成全部大写。
;;  ~)
;;  大小写转换结束。

;;  如何让 guile 的命令行支持 readline?
;; (use-modules (ice-9 readline))
;; (activate-readline)

;;  如何“重载”函数名？

;;  如果你不想用 OOP 但是想根据参数个数来重载函数名，可以用 case-lambda。它在 SRFI-16 里。你可以这样使用：
;; (use-modules (srfi srfi-16))

;; (define foo (case-lambda
;;               ((x) #t)
;;               ((x y) (+ x y))
;;               (z
;;                 (apply * z))))
;; (foo 'bar)
;; (foo 2 4)
;; (foo 3 3 3)

;;  如何方便的操作 string?
;;  SRFI-13 有很多方便的操作 string 的函数。 比如：
;; string-any pred s [start end]
;; string-every pred s [start end]

;;  用于判断字符串里的字符是否满足 pred 设定的条件。
;; string-tabulate proc len

;;  制造一个长度 len 的 string, 对每一个位置使用 proc 来初始化。
;; string-join ls [delimiter grammar]

;;  把 list ls 里的字符组合成一个字符串。使用分解符 delimiter, 使用语法 grammar。grammar 可以是 infix, string-infix, suffix, 或者 prefix.
;; string-copy str [start end]

;;  拷贝子字符串。
;; substring/shared str start [end]

;;  也是拷贝。但是共享空间。
;; string-copy! target tstart s [start end]

;;  把 s 里 [start, end) 之间的部分拷贝到 target 里从 tstart 开 始的区域。target 和 s 可以是同一个 string.
;; string-fill! str chr [start end]
;; string-index s char_pred [start end]

;;  在 s 里寻找 char-pred, 如果 char-pred 是一个字符。否则在 s 里寻找满足 char-pred 函数的位置。
;; string-contains s1 s2 [start1 end1 start2 end2]
;; string-contains-ci s1 s2 [start1 end1 start2 end2]

;;  在 s1 里寻找 s2.
;; string-replace s1 s2 [start1 end1 start2 end2]

;;  把 s1 里的一部分换成 s2 里的一部分。
;; string-filter s char_pred [start end]

;;  把 s 里的字符通过 char_pred 过滤。
;; string-delete s char_pred [start end]

;;  作用正好跟 string-filter 相反。

;;  比如：
;; (define (p1 x)
;;   (char-upper-case? x))

;; (string-filter "kiHck!shAit*hLaLaldsjfOha" p1)

;;  结果是 "HALLO".
;; string-tokenize s [token_char start end]

;;  把一个string切分成几个，用 token_char 作为分割。如果没有 token_char 就用空白作为分割。比如：
;; (string-tokenize "ha kick flip")

;;  结果是 ("ha" "kick" "flip").
;;  如何方便的操纵 list?

;;  SRFI-1 里有很多函数用来操作 list.
;;  怎样用 string 作为 port?

;;  SRFI-6 提供了 string port 的支持。

;;  open-input-string, open-output-string and get-output-string
;;  怎样定义 record？

;;  SRFI-9 提供 define-record-type. 这跟 Scheme 48 的 record一样。

;;  比如：
;; guile> (use-modules (srfi srfi-9))
;; guile> (define-record-type :foo (make-foo x) foo?
;;                            (x get-x) (y get-y set-y!))
;; guile> (define f (make-foo 1))
;; guile> f
;; #<:foo x: 1 y: #f>
;; guile> (get-x f)
;; 1
;; guile> (set-y! f 2)
;; 2
;; guile> (get-y f)
;; 2
;; guile> f
;; #<:foo x: 1 y: 2>
;; guile> (foo? f)
;; #t
;; guile> (foo? 1)
;; #f
;;  怎样进行同时的 list 赋值？

;;  SRFI-11 提供了 let-values.
;; (use-modules (srfi srfi-11))
;; (let-values (((x y) (values 1 2))
;;              ((z f) (values 3 4)))
;;    (+ x y z f))
;;  怎样把复杂的数据结构漂亮的显示出来？

;;  使用 (ice-9 pretty-print) 就行。比如：
;; (use-modules (ice-9 pretty-print))
;; (pretty-print '(define (foo) (lambda (x)
;; (cond ((zero? x) #t) ((negative? x) -x) (else (if (= x 1) 2
;; (* x x x)))))))

;;  结果是：
;;   (lambda (x)
;;     (cond ((zero? x) #t)
;;           ((negative? x) -x)
;;           (else (if (= x 1) 2 (* x x x))))))
;;  怎样使用 value histroy?
;; (use-modules (ice-9 history))
;; guile> 1
;; $1 = 1
;; guile> (+ $1 $1)
;; $2 = 2
;; guile> (* $2 $2)
;; $3 = 4

;;  怎样在出错时自动显示 backtrace?
;; (debug-enable 'backtrace)

;;  怎样改变 guile 的 REPL?
;; Scheme Procedure: read-enable option-symbol
;; Scheme Procedure: print-enable option-symbol
;; Scheme Procedure: debug-enable option-symbol
;; Scheme Procedure: trap-enable option-symbol 

;; Scheme Procedure: read-disable option-symbol
;; Scheme Procedure: print-disable option-symbol
;; Scheme Procedure: debug-disable option-symbol
;; Scheme Procedure: trap-disable option-symbol 

;; syntax: read-set! option-symbol value
;; syntax: print-set! option-symbol value
;; syntax: debug-set! option-symbol value
;; syntax: trap-set! option-symbol value 
;;  Read:
;; keywords         #f      Style of keyword recognition: #f or 'prefix
;; case-insensitive no      Convert symbols to lower case.
;; positions        yes     Record positions of source code expressions.
;; copy             no      Copy source code expressions.
;;  Eval
;; stack           22000   Size of thread stacks (in machine words).
;;  Print
;; source          no      Print closures with source.
;; closure-hook    #f      Hook for printing closures.
;;  怎样改变 debugger 的行为？
;; stack           20000   Stack size limit (0 = no check).
;; debug           yes     Use the debugging evaluator.
;; backtrace       no      Show backtrace on error.
;; depth           20      Maximal length of printed backtrace.
;; maxdepth        1000    Maximal number of stored backtrace frames.
;; frames          3       Maximum number of tail-recursive frames in backtrace.
;; indent          10      Maximal indentation in backtrace.
;; backwards       no      Display backtrace in anti-chronological order.
;; procnames       yes     Record procedure names at definition.
;; trace           no      *Trace mode.
;; breakpoints     no      *Check for breakpoints.
;; cheap           yes     *Flyweight representation of the stack at traps.

;;  怎样从一个外部程序得到输入，就像 popen?

;;  使用 (ice-9 popen) module:
;; (use-modules (ice-9 popen))
;; (define p (open-input-pipe "ls -l"))
;; (read-line p)

;;  guile 有没有方便的排序函数？
;;  试试这些：
;; (define a '(2 3 45 5 6))
;; (define b '(83 3 59 18 38))

;; (sorted? a <)

;; (define as (sort a <))
;; (define bs (sort b <))

;; (merge as bs <)

;; (sort! a <)

;; (stable-sort b <)

;; (define v1 '#(23 4 5 8 34 83 23 18 35))

;; (sort v1 <)

;; (restricted-vector-sort! v1 < 4 7)

;;  怎样深度 copy 一个对象？
;;  copy-tree.
;;  怎样把一个对象转换成 string?

;;  object->string
;; (object->string fact1)
