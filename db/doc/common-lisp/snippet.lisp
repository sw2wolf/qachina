(defmacro -> (initial &rest args)
    (let ((first (first args)))
      (cond ((null args) initial)
            ((null (rest args))
             `(,(car first) ,initial ,@(rest first)))
            (:else
             `(-> (-> ,initial ,first) ,@(rest args))))))
 
->
CL-USER> (-> 10
               (+ 20)
               (+ 40)
               (/ 10))
 
(ql:quickload :infix-dollar-reader)
(syntax:use-syntax :infix-dollar)
(= (+ 1 2 $ * 3 4 $ + 5 6) (+ 1 2 (* 3 4 (+ 5 6)))) ; => T
 
Implementation is as below;
(defun infix-dollar-reader (stream char)
    (declare (ignore char))
    (let* ((lp-reader (get-macro-character #\())
           (entity (funcall lp-reader stream #\()) )
      (unread-char #\) stream)
      entity ))

(dolist (x '(1 2 3)) (print x) (if (evenp x) (return)))   ;通过return截断了循环

(defun load-db (filename) 
    (with-open-file (in filename) 
        (with-standard-io-syntax (setf *db* (read in)))))

(with-open-file (file-stream #p "somefile" :direction :output)
    (let ((*standard-output* file-stream)) (print "This prints to the file, not stdout."))
    (print "And this prints to stdout, not the file."))

(defun split (string)
    (let ((space-position (position #\Space string)))
        (list 
            (subseq string 0 space-position) 
            (subseq string (+ space-position 1)))))

(let* ((space-position (position #\Space string))
       (first (parse-integer string :end space-position))
       (second (parse-integer string :start (1+ space-position)))
       (* first second)))

(parse-integer "123")

(defun infinite-loop () (loop :until *nil*))
(loop :repeat n :for x :from 0 :collect (* 2 x))

(with-open-file (stream "/etc/passwd")
    (loop for line = (read-line stream nil)
        until (null line)
        do (print line)))

(sort (list '(9 a) '(3 b) '(4 c))
     #'(lambda (x y) (< (car x) (car y))))

(defun dumb-string-hash (string)
  "Produce a six-character hash of STRING."
    (let ((hash #xD13CCD13))
        (loop for char across string
              for value = (char-code char)
              do
                (setf hash (logand #xFFFFFFFF (logxor (ash hash 5) (ash hash -27) value))))
        (subseq (format nil "~(~36,6,'0R~)" (mod hash 88888901)) 0 6)))

(with-input-from-string (s "1 2 3 4 5 6 7" :index i :start 0 :end 13)
                  (list (read s) (read s) (read s) (read s) (read s) (read s)))
(with-input-from-string (s "1 2 3 4 5 6 7") (loop for x = (read s nil :end) until (eq x :end) collect x))

(defun escape-for-html (string)
  "Escapes the characters #\\<, #\\>, #\\', #\\\", and #\\& for HTML output."
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
            while char
            do (case char
                 ((#\<) (write-string "&lt;" out))
                 ((#\>) (write-string "&gt;" out))
                 ((#\") (write-string "&quot;" out))
                 ((#\') (write-string "&#039;" out))
                 ((#\&) (write-string "&amp;" out))
                 (otherwise (write-char char out)))))))

(or (char<= #\A char #\Z) (char<= #\a char #\z))

(let ((string "1 2 3"))
    (loop :for (integer position) := (multiple-value-list (parse-integer string :start (or position 0) :junk-allowed t))
    :while integer :collect integer))

;do宏是Common Lisp中最基本的迭代操作符. 就象let,do也会产生变量,它的第一个自变量是关于变量规格的表. 表中的每个元素具有如下形式: 
;        (variable initial update)
(defun show-squares (start end)
    (do ((i start (+ i 1)))
        ((> i end) 'done)
        (format t "~A ~A~%" i (* i i))))

(macroexpand '(setf foo 42))

(defun same-bag-p (bag1 bag2 &key (test #'eql))
    (let ((table (make-hash-table :test test)))
         (loop for key in bag1 do (incf (gethash key table 0)))
         (loop for key in bag2 do (decf (gethash key table 0)))
         (loop for val being each hash-value of table always (= val 0))))

(loop with buffer = (make-array +buffer-length+ :element-type 'octet)
      with content = (make-array 0 :element-type 'octet :adjustable t)
      for index = 0 then (+ index pos)
      for pos = (read-sequence buffer content-stream)
      do (adjust-array content (+ index pos))
         (replace content buffer :start1 index :end2 pos)
      while (= pos +buffer-length+)
      finally (return content))

(make-hash-table :test #'equal :size 4000)

(setq table (make-hash-table))
(dotimes (i 10) (setf (gethash i table) i))
(let ((sum-of-squares 0))
    (maphash #'(lambda (key val) 
        (let ((square (* val val)))
            (incf sum-of-squares square)
            (setf (gethash key table) square))) table)
    sum-of-squares)
(hash-table-count table) => 10
(maphash #'(lambda (key val)
    (when (oddp val) (remhash key table))) table)
(hash-table-count table) =>  5
(maphash #'(lambda (k v) (print (list k v))) table)


(loop for key being the hash-keys of *my-hash* do (print key))

(defvar *unicode-test-file* "faithtest-out.txt")

(defun generate-256 (&key (filename *unicode-test-file*) #+CLISP (charset 'charset:iso-8859-1) external-format)
    (let ((e (or external-format #+CLISP (ext:make-encoding :charset charset :line-terminator :unix))))
        (describe e)
        (with-open-file (f filename :direction :output :external-format e)
            (write-sequence
                (loop with s = (make-string 256)
                    for i from 0 to 255
                        do (setf (char s i) (code-char i))
                        finally (return s)) f) (file-position f))))
;(generate-256 :external-format :default)
;#+CLISP (generate-256 :external-format :unix)
;#+CLISP (generate-256 :external-format 'charset:ascii)
;(generate-256)

(make-string 7 :initial-element #\Space)

(defun check-256 (&optional (filename *unicode-test-file*))
    (with-open-file (f filename :direction :input :element-type '(unsigned-byte 8))
        (loop for i from 0
            for c = (read-byte f nil nil)
            while c
            unless (= c i)
            do (format t "~&Position ~D found ~D(#x~X)." i c c)
                when (and (= i 33) (= c 32))
                    do (let ((c (read-byte f)))
                             (format t "~&Resync back 1 byte ~D(#x~X) - cause CRLF?." c c)))
        (file-length f)))

(let ((buf (make-array 4096 :element-type (stream-element-type input-stream)))
  (loop for pos = (read-sequence input-stream)
          while (plusp pos)
                  do (write-sequence buf output-stream :end pos))))

(defvar *data-float* ())
(defvar *data-integer* ())
(defvar *data-others* ())

(defun read-data (file)
    (declare (type pathname file))
    (with-open-file (s file :direction :input :element-type 'character)
        (let ((*read-default-float-format* 'double-float))
            (handler-case
                (loop for i = (read s)
                    do (typecase i
                        (double-float (push i *data-float*))
                        (integer (push i *data-integer*))
                        (otherwise (push i *data-others*))))
                (end-of-file ())))))

(let* ((rs1 (make-random-state nil))
       (rs2 (make-random-state t))
       (rs3 (make-random-state rs2))
       (rs4 nil))
    (list
        (loop for i from 1 to 10 
            collect (random 100)
            when (= i 5)
            do (setq rs4 (make-random-state)))
        (loop for i from 1 to 10 collect (random 100 rs1))
        (loop for i from 1 to 10 collect (random 100 rs2))
        (loop for i from 1 to 10 collect (random 100 rs3))
        (loop for i from 1 to 10 collect (random 100 rs4))))
=>
((29 25 72 57 55 68 24 35 54 65)
(29 25 72 57 55 68 24 35 54 65)
(93 85 53 99 58 62 2 23 23 59)
(93 85 53 99 58 62 2 23 23 59)
(68 24 35 54 65 54 55 50 59 49))

(loop :for elt :in *list*
    :collect (log elt) :into logs
    :finally (return 
        (loop :for l :in logs
            :if (> l 5.0) :collect l :into ms
            :else :collect l :into ns
            :finally (return (values ms ns)))))
(6.089045 6.7569323 6.364751 6.514713)
(4.787492)

(defun facX (n)
  (funcall 
      (lambda (fn n) 
          (funcall fn n fn))
      (lambda (n this) 
          (cond
              ((> n 0) (* n (funcall this (- n 1) this)))
              (t 1))) n))

(dolist (k '(1 2 3 :four #\v () t 'other))
    (format t "~S "
        (case k ((1 2) 'clause1)
                (3 'clause2)
                (nil 'no-keys-so-never-seen)
                ((nil) 'nilslot)
                ((:four #\v) 'clause4)
                ((t) 'tslot)
                (otherwise 'others))))
(defvar *tmp-test-directory*
    #+(or :win32 :mswindows) #p"c:\\hunchentoot-temp\\test\\"
    #-(or :win32 :mswindows) #p"/tmp/hunchentoot/test/")

(let ((string (make-array (file-length in)
        :element-type #-:lispworks 'character #+:lispworks 'lw:simple-char :fill-pointer t)))
    (setf in (flex:make-flexi-stream in :external-format *utf-8*)
        (fill-pointer string) (read-sequence string in)) string)

(setq file-name (cl-ppcre:regex-replace ".*\\\\" "c:\\temp\test.dat" "")) =>  "temptest.dat"

(defun parse-host-name-and-port (host-and-port)
  (let ((strings
         (nth-value 1
                    (cl-ppcre:scan-to-strings "^([^:]*)(:([^:]*))?$"
                                              host-and-port))))
    (values (elt strings 0)
            (elt strings 2))))

(cl-ppcre:split ": " line :limit 2)
(cl-ppcre:scan-to-strings "((a+)(b))" "cccaaaabx") 会得到 "aaaab" 和 #("aaaab" "aaaa" "b"), 第一个是匹配的串,第二个是 capture 到的串组.

(defun game-from-name (name)
    (find name *games* :test #'string-equal :key  #'name))

(defun games ()
     (sort (copy-list *games*) #'> :key #'votes))
(sort (push head (screen-heads screen)) #'< :key 'head-number)

(defun stream-direct ()
    (setf (content-type*) "text/html; charset=utf-8")
    (let ((stream (send-headers))
          (buffer (make-array 1024 :element-type 'flex:octet)))
        (with-open-file (in *utf-8-file* :element-type 'flex:octet)
            (loop for pos = (read-sequence buffer in)
                until (zerop pos) 
                do (write-sequence buffer stream :end pos)))))

(typecase new-value
 (null
  (delete-file-if-exists file))
 (string
  (ensure-directories-exist file)
  (with-open-file (stream file :direction :output
                          :if-does-not-exist :create
                          :if-exists :rename-and-delete)
    (write-line new-value stream)))
 (t
  (error "Bad config value ~S; must be a string or NIL" new-value)))

(defun parse-date (string)
    "Parse a date string in the form YYYY-MM-DD and return the
    year, month, and day as multiple values."
    (values (parse-integer string :start 0 :end 4)
            (parse-integer string :start 5 :end 7)
            (parse-integer string :start 8 :end 10)))

* (/ 22.0 7.0)
3.142857
* (setf *read-default-float-format* 'double-float)
DOUBLE-FLOAT
* (/ 22.0 7.0)
3.142857142857143

; (split "The quick brown fox.") =>("The" "quick" "brown" "fox.")
(defun split (string &optional (split-character #\Space))
    (let ((result '())
          (stream (make-string-output-stream)))
        (loop for char across string
            if (char= char split-character)
                do (push (get-output-stream-string stream) result)
            else
                do (write-char char stream))
        (push (get-output-stream-string stream) result)
        (nreverse result)))

(case char
    (#\Space
       (save)
       #'in-space)
    (t
       #'in-word))

(when (every #'digit-char-p version-string)
        (values (parse-integer version-string)))
(every (lambda (elt) (= elt 910)) b)

(every (lambda (char)
           (or (digit-char-p char)
               (eql char #\.)))
         string)

(declaim (inline whitespacep))
(defun whitespacep (c)
  "Checks whether C is a whitespace character."
  (find c '(#\Space #\Tab #\Newline #\Linefeed #\Return #\Page)))

(defun rfc-1123-date (&optional (time (get-universal-time)))
  "Generates a time string according to RFC 1123.  Default is current time."
  (multiple-value-bind
        (second minute hour date month year day-of-week)
      (decode-universal-time time 0)
    (format nil "~A, ~2,'0d ~A ~4d ~2,'0d:~2,'0d:~2,'0d GMT"
            (svref +day-names+ day-of-week)
            date
            (svref +month-names+ (1- month))
            year
            hour
            minute
            second)))

(reduce #'max index-value-list :key #'car :initial-value -1)

(loop with buf = (make-array +buffer-length+ :element-type 'octet)
              for pos = (read-sequence buf file)
              until (zerop pos)
              do (write-sequence buf out :end pos)
                 (finish-output out))

(map '(vector (unsigned-byte 8) *) 'char-code "abc123")
=>#(97 98 99 49 50 51)

(map 'string #'(lambda (x y) (char "01234567890ABCDEF" (mod (+ x y) 16)))
    '(1 2 3 4) '(10 9 8 7))
=>AAAA

(map 'string #'(lambda (x) (if (oddp x) #\1 #\0)) '(1 2 3 4))
=>1010

(map 'list #'(lambda (x) (format nil "~D" x)) '(1 2 3))
("1" "2" "3")

(setf x (make-array '(3 5) :initial-element 3))
=>  #2A((3 3 3 3 3) (3 3 3 3 3) (3 3 3 3 3))

(make-array *max-buffer-size* :element-type '(unsigned-byte 8) :initial-element 0))

(nth-value 0 (read-from-string "qqq"))

(format nil "~12d" 1000000)    ==> "     1000000"
(format nil "~12,'0d" 1000000) ==> "000001000000"

* (proclaim '(optimize (debug 3)))
* (defun foo (a b) (* (+ a b) b))
* (step (foo 1 2))
START Selects the CONTINUE restart if one exists and starts
single-stepping. Single stepping affects only code
compiled with under high DEBUG optimization quality.
See User Manual for details.

STEP  Steps into the current form.
NEXT  Steps over the current form.
OUT   Stops stepping temporarily, but resumes it when the topmost
      frame that was stepped into returns.
STOP  Stops single-stepping.

(setf (symbol-function funct-name) (lambda (x) (+ x 1)))

; 设置 hunchentoot 编码  
(defvar *utf-8* (flex:make-external-format :utf-8 :eol-style :lf))
(setq hunchentoot:*hunchentoot-default-external-format* *utf-8*)

(defun join (list &optional (delim "&"))
    (with-output-to-string (s)
    (when list
        (format s "~A" (first list))
        (dolist (element (rest list))
            (format s "~A~A" delim element)))))

(defconstant +crlf+
  (make-array 2 :element-type '(unsigned-byte 8)
              :initial-contents (mapcar 'char-code '(#\Return #\Linefeed)))
  "A 2-element array consisting of the character codes for a CRLF sequence.")

(defstruct node 
    (board            (empty-board))  
    (tomove           'white)         
    (last-moves       nil)
    (castling-white   (make-castling-data))
    (castling-black   (make-castling-data))
    (ep               nil)
    (50-moves-counter 0)
    (moves            nil))

(defstruct foo bar baz qux)
(with-accessors ((r foo-bar) (z foo-baz) (x foo-qux))
    (make-foo :bar 1 :baz 2 :qux 3)
    #.`(progn
        ,@(loop for a in '(r z x)
            collect `(incf ,a)))
    (list r z x))
(with-accessors ((r foo-bar) (z foo-baz) (x foo-qux))
    (make-foo :bar 1 :baz 2 :qux 3)
    `(progn
        ,@(loop for a in '(r z x)
            collect `(incf ,a)))
    (list r z x))

(map nil
    (lambda (function argument)
        (funcall function argument object))
    (list #'(setf foo) #'(setf bar))
    (list arg1 arg2))

;Make a subclass and specialize that way:
(defclass my-acceptor (hunchentoot:acceptor) ())
(defmethod session-cookie-name ((acceptor my-acceptor)) "my-session")

(defclass foo ()
    ((bar :initarg :bar :accessor foo-bar)
     (baz :initarg :baz :accessor foo-baz)
     (qux :initarg :qux :accessor foo-qux)))
(loop
    :with instance = (make-instance 'foo :bar 1 :baz 2 :qux 3)
    :for accessor :in '(foo-bar foo-baz foo-qux)
    :do (funcall (fdefinition (list 'setf accessor)) (1+ (funcall accessor instance)) instance)
    :finally (inspect instance))

(let ((result-string (make-sequence 'simple-string total-size))
          (curr-pos 0))
      (dolist (string string-list)
        (replace result-string string :start1 curr-pos)
        (incf curr-pos (length string)))
      result-string)

(defun http-cookie-value-p (value)
    "Tests whether VALUE is a string which is a valid cookie-value according to RFC 6265"
    (and (stringp value)
         (not (some (lambda (char)
                        (let ((cc (char-code char)))
                            (or (< cc #x21)
                                (= #x22 cc)
                                (= #x2c cc) 
                                (= #x3b cc)
                                (= #x5c cc)
                                (> cc #x7e))))
                    value))))

(defun normalize-whitespace (str)
  (substitute #\Space #\Newline (substitute #\Space #\Tab str)))

(defun trim-spaces (str)
  (string-trim '(#\Space) str))

(string-trim '(#\Return) line)

(defmacro define-trivial-special-ops (&rest mappings)
  `(progn ,@(loop for (form-name js-primitive) on mappings by #'cddr collect
                 `(define-expression-operator ,form-name (&rest args)
                    (cons ',js-primitive (mapcar #'compile-expression args))))))

(define-trivial-special-ops
  array      ps-js:array
  instanceof ps-js:instanceof
  typeof     ps-js:typeof
  new        ps-js:new
  delete     ps-js:delete
  in         ps-js:in ;; maybe rename to slot-boundp?
  break      ps-js:break
  <<         ps-js:<<
  >>         ps-js:>>
  )

(defun hash-keys (hash)
  (loop for k being the hash-keys of hash
     collect k))
(defun hash-values (hash)
  (loop for k being the hash-values of hash
     collect k))

(make-instance 'hunchentoot:easy-ssl-acceptor :port 8083
               :ssl-privatekey-password "xxxxxxx"
               :ssl-certificate-file  "~/git/config/https-cert/server.crt"
               :ssl-privatekey-file "~/git/config/https-cert/server.key")

(setf master-socket
    (usocket:socket-listen host port
        :reuse-address t
        :element-type 'unsigned-byte))
(usocket:wait-for-input master-socket)
(setf new-client (usocket:socket-accept master-socket))
(format (usocket:socket-stream new-client) "Hello~%")

(setf sockets (list master-socket))
;; see below for the :ready-only description
(loop
    (loop :for s :in (wait-for-input sockets :ready-only t) :do
        (if (eq s master-socket)
        ;; THEN: new connection
            (let ((new (usocket:socket-accept s)))
            ;; add the new socket to the list of sockets that we're listening to
                (setf sockets (nconc sockets `(,new)))
                (handle-client-connect new))
        ;; ELSE: data from a connected client
            (handle-client-input s))))

(defun collect-input (socket buffer &optional (end-char 0))
    (loop
        :with stream = (socket-stream socket)
        :with byte
        :while (listen stream)
        :doing
            (setq byte (read-byte stream))
            (when (= byte end-char)
                (return t))
            (vector-push-extend byte buffer)))
(defun reset-buffer (client)
  (setf (fill-pointer (client-buffer client)) 0))

(defgeneric client-read (client socket)
    (:method ((client client) socket)
        (with-slots (buffer) client
            (when (collect-input socket buffer)
                (prog1
                    (utf-8-bytes-to-string buffer)
                    (reset-buffer client))))))

(defgeneric handle-client-input (server socket)
    (:method ((server server) socket)
        (with-slots (connections) server
            (let ((client (gethash socket connections)))
                (awhen (client-read client socket)
                (send-to-workers server (curry #'client-on-command client it)))))))

(defmacro awhen (cond &body body)
  `(let ((it ,cond))
       (when it
              ,@body)))

(defun curry (fun &rest args1)
  (lambda (&rest args2)
      (apply fun (append args1 args2))))

(let ((out (flexi-streams:make-flexi-stream
			(tbnl:send-headers)
			:external-format tbnl::+utf-8+)))
	      (copy-stream in out 'character))

(defun run-interactive (command &key args)
    (let ((proc (sb-ext:run-program command args
        :input :stream
        :output :stream
        :wait nil)))
        (list proc (sb-ext:process-input proc) (sb-ext:process-output proc))))

(let ((cl-who:*prologue* "<!DOCTYPE html>"))                                
    (cl-who:with-html-output-to-string (*standard-output* nil :prologue t)    
             (:html (:body "Not much here, just HTML5 Test"))))

(defun analytics-service ()
  (save-parsed-events
     (dom:first-child
         (flexi-streams:with-input-from-sequence
         (stream (hunchentoot:raw-post-data
          :request hunchentoot:*request*
           :force-binary t))
                 (cxml:parse-stream
                        stream (cxml-dom:make-dom-builder)))))
                          "ok")
(coerce '(#\u2211) 'string)
(uffi:default-foreign-library-type)
"so"

(defun triplets (list)
  (loop while list
          collect (list (pop list)
                        (pop list)
                        (pop list))))
>(triplets '(a b c d e f g h i))
((A B C) (D E F) (G H I))

(defmacro with-restarts-menu (&body body)
  "Execute BODY. If an error occurs allow the user to pick a
restart from a menu of possible restarts. If a restart is not
chosen, resignal the error."
  (let ((c (gensym)))
    `(handler-bind
         ((warning #'muffle-warning)
          ((or serious-condition error)
           (lambda (,c)
             (restarts-menu ,c)
             (signal ,c))))
       ,@body)))

(unwind-protect
    (handle-keymap (remove-if-not 'kmap-or-kmap-symbol-p bindings) code state key-seq nil update-fn)
    (when grab (ungrab-pointer)))

(handler-case
      (xlib:atom-name *display* n)
    (xlib:atom-error ()
      nil))
(asdf:system-relative-pathname 'my-system "files/data.txt")

;coverage
(require :sb-cover)
(declaim (optimize sb-cover:store-coverage-data))
(asdf:oos 'asdf:load-op :cl-geonames-test)
(cl-geonames-test:run-cl-geonames-tests)
(sb-cover:report "/tmp/cl-geonames-coverage/")
(declaim (optimize (sb-cover:store-coverage-data 0)))

(loop for code in codes
    as i = 1 then (1+ i)
    do (format os "&adminCode~A=~A" i code))

(dolist (sys '(weblocks weblocks-demo))
  (asdf:oos 'asdf:load-op sys))
(use-package :weblocks)
(use-package :cl-who)
(save-lisp-and-die #p"weblocks.sbclcore")

(check-type secs (real 0 *))
(check-type repeat (or null (real 0 *)))
(check-type function (or function symbol))

(let ((timer (make-timer
                :repeat repeat
                :function function
                :args args)))
    (schedule-timer timer secs)
    (setf *timer-list* (sort-timers (cons timer *timer-list*)))
    timer)
(defun schedule-timer (timer when)
  (setf (timer-time timer) (+ (get-internal-real-time)
                              (* when internal-time-units-per-second))))

(now (/ (get-internal-real-time) internal-time-units-per-second))

(defun grep (regexp filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil nil)
            while line
            do (when (scan regexp line)
                (format t "~A~%" line)))))

(setq my-socket (ccl:make-socket :connect :passive :format :text
        :local-port 20000 :reuse-address t))
(setq connection (ccl:accept-connection my-socket))
(setq remote-host (ccl:remote-host connection))
(setq remote-port (ccl:remote-port connection))

(setf *mpd-socket*
   (handler-case
       #+clisp
       (socket:socket-connect *mpd-port* *mpd-server* :element-type 'character)
       #+sbcl
       (let ((s (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
           (sb-bsd-sockets:socket-connect s *mpd-server* *mpd-port*)
           (sb-bsd-sockets:socket-make-stream s
               :input t :output t :buffering :none))
       #+ccl
       (let ((s (ccl:make-socket :connect :active :format :text
               :remote-host *mpd-server*
               :remote-port *mpd-port*)))
           (setf (stream-external-format s) :utf-8)
           s)
       (error (err)
           (format t "Error connecting to mpd: ~a~%" err))))

(in-package #:quicklisp-client)
(defun system-apropos* (term)
    (let (result)
        (dolist (system (provided-systems t))
            (when (or (search term (name system))
                    (search term (name (release system))))
                        (push system result)))
                            (nreverse result)))
                             
(defun quickload* (systems &key verbose prompt)
  (unless (consp systems)
      (setf systems (list systems)))
        (dolist (system systems)
            (if (typep system 'system)
               (ql:quickload (name system) :verbose verbose :prompt prompt)
               (ql:quickload system :verbose verbose :prompt prompt))))
                    
(export 'system-apropos* 'quicklisp-client)
(export 'quickload* 'quicklisp-client)
;; example :
;; we can load all lispbuilder systems
(ql:quickload* (ql:system-apropos* "lispbuilder-"))

(lambda (c stream)
   (typecase c
     (simple-condition
      (format stream
              (simple-condition-format-control (usocket-real-error c))
              (simple-condition-format-arguments (usocket-real-error c))))
     (otherwise
      (format stream "The condition ~A occurred." (usocket-real-error c)))))


(make-pathname :defaults *contrib-dir*
   		       :name :wild
			   :type "lisp")

(load (make-pathname :host (pathname-host *base-dir*)
		     :device (pathname-device *base-dir*)
		     :directory (append (pathname-directory *base-dir*) (list "contrib"))
		     :name "asdf" :type "lisp"))
;It uses the nice 2-complement facts that bit-anding a number and its opposite returns the least significant set bit
;and that bit-anding a number and one less than the number zeroes this least significant set bit.
(defmacro do-bits ((var x) &rest body)
  "Evaluates [body] forms after binding [var] to each set bit in [x]"
    (let ((k (gensym)))
        `(do ((,k ,x (logand ,k (1- ,k))))
                 ((= ,k 0))
                        (let ((,var (logand ,k (- ,k))))
                                 ,@body))))

(loop for i below (integer-length 109)
      collect (if (logbitp i 109) 1 0))

(loop for i from src-start to src-end
      for j from dst-start
      as c = (char-code (char src i))
      if (<= min c max) do (setf (aref dst j) c)
      ;; replace unknown characters with question marks
      else do (setf (aref dst j) (char-code #\?))
      finally (return i))

;How do you test if an object is a cons that has the desired symbol in the car?
(typep x '(cons (eql :foo)))
or
(and (consp x) (eq :foo (car x)))

(defun my-foo () )
MY-FOO
(nth-value 2 (function-lambda-expression #'my-foo))
MY-FOO

(loop for s in *screen-list*
      nconc (delete-if 'window-hidden-p (copy-list (group-windows (screen-current-group s)))))

(defmacro define-window-slot (attr)
  "Create a new window attribute and corresponding get/set functions."
  (let ((win (gensym))
        (val (gensym)))
    `(progn
      (defun ,(intern1 (format nil "WINDOW-~a" attr)) (,win)
        (gethash ,attr (window-plist ,win)))
      (defun (setf ,(intern1 (format nil "WINDOW-~a" attr))) (,val ,win)
        (setf (gethash ,attr (window-plist ,win))) ,val))))

;The :accessor slot option defines two functions: FOO to read the slot value and (SETF FOO) to set the slot value. Note that in the latter case in Common Lisp the function name is not a symbol, but a list.

;If you want to have a list of functions and values, then your list needs to contain the setter functions.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :cl-rmath) (setf *read-default-float-format* 'double-float))
(declare (fixnum N thin))
(declare (optimize (speed 3) (safety 1)))
(declare (type (simple-array (unsigned-byte 8) *) buffer))
(declare (double-float x y))
(declare (ignore argv))
(declare (type single-float deg) (optimize (speed 3) (safety 0) (debug 0)))

(setf x (cl-rmath::rgamma 3.0 (/ 1.0 (+ (* y y) 4))))
(setf y (cl-rmath::rnorm (/ 1.0 (+ x 1.0)) (/ 1.0 (sqrt (+ (* 2 x) 2)))))

;buildapp --output gibbs --asdf-tree /usr/share/common-lisp/source/ --asdf-tree /usr/local/share/common-lisp/source/ --load-system cl-rmath --load gibbs.lisp --entry main

(defun compose2 (f g) (lambda (x) (funcall g (funcall f x))))

(loop for index of-type (integer 0 16) from 0 below 16
 do (setf (aref block index) #x00000000))

;(stumpwm-system::*components* is my addition to stumpwm.asd, used to generate the ASDF components in that file and the properly-ordered list of files above.)
(defun system-objects (system)
  (loop for component in (asdf:module-components (asdf:find-system system))
      for pathname = (asdf:component-pathname component)
      for directory = (pathname-directory pathname)
      for name = (pathname-name pathname)
      when (equal "lisp" (pathname-type pathname))
        collect (make-pathname :directory directory :type "o" :name name)))

(c:build-program "stumpwm" :lisp-files
  (concatenate 'list
      (system-objects :cl-ppcre)
      (system-objects :clx)
      (mapcar (lambda (component)
        (concatenate 'string component ".o"))
        stumpwm-system::*components*))
  :epilogue-code '(unwind-protect (stumpwm:stumpwm) (ext:quit)))

CL-USER> (let* ((string "Hello World!")
                (c-string (cffi:foreign-funcall "strdup" :string string :pointer)))
            (unwind-protect (write-line (cffi:foreign-string-to-lisp c-string))
                (cffi:foreign-funcall "free" :pointer c-string :void))
            (values))
Hello World!

CL-USER> (cffi:load-foreign-library "libX11.so")
#<CFFI::FOREIGN-LIBRARY {1004F4ECC1}>
CL-USER> (cffi:foreign-funcall "XOpenDisplay"
            :string #+sbcl (sb-posix:getenv "DISPLAY")
            #-sbcl ":0.0"
            :pointer)
#.(SB-SYS:INT-SAP #X00650FD0)

(with-open-file (file "/home/simkoc/test.pdf"
    :direction :io
    :if-does-not-exist :create
    :if-exists :supersede
    :element-type '(unsigned-byte 8))
    (let ((input (drakma:http-request "http://www.fractalconcept.com/ex.pdf"
        :want-stream t)))
    (while (read-byte input nil nil) ;set eof-error-p and eof-value to nil
        (write-byte it file))
        (close input)))

;somefile.lisp
(set-macro-character #\] (get-macro-character #\)))
(set-dispatch-macro-character #\# #\[
  (lambda (stream char1 char2)
      (declare (ignore char1 char2))
          (setf (readtable-case *readtable*) :preserve)
          (unwind-protect
             (let ((command-line (read-delimited-list #\] stream t)))
                  (list 'ext:run-program (princ-to-string (car command-line))
                         :arguments `',(mapcar #'princ-to-string (rest command-line))))
                   (setf (readtable-case *readtable*) :upcase))))
;clisp -K full -i somefile.fas
clisp -K full -x "(load \"asdf.lisp\") (load \"stumpwm.asd\") (load \"/usr/share/common-lisp/systems/cl-ppcre.asd\") (asdf:operate 'asdf:load-op :stumpwm) (stumpwm::stumpwm)"

(require 'asdf)
(asdf:oos 'asdf:load-op 'stumpwm)

(rename-package "CL-PPCRE" "CL-PPCRE" '("PPCRE" "RE"))

(defun repl ()
    (princ "> ")
    (loop 
        (shiftf *** ** * (eval (read))) 
        (format t "~a~&> " *)))

(defparameter *primes* (map-into 
    (make-array 100 :element-type 'fixnum :adjustable t :fill-pointer 0)
    #'identity '(2 3)))

;trick to enforce provision of required slot values for structures and classes:
(defstruct foo
  (slot1 (error "Must provide value for slot1")))

(time (reduce #'+ (make-list 100000 :initial-element 1)))

(format s "~VR" 16 13) => D
(format s "~2,'0x" 13) => 0D

(let ((server (iolib:make-socket :connect :passive
				   :address-family :internet
				   :type :stream
				   :ipv6 nil)))

    (format t "Created socket: ~A[fd=~A]~%" server (socket-os-fd server))

    (bind-address server 
		  (or (acceptor-address acceptor) 
			     +ipv4-unspecified+) 
		  :port port 
		  :reuse-addr t)
    (format t "Bound socket: ~A~%" server)

    (listen-on server
	       :backlog (acceptor-listen-backlog acceptor))
    (format t "Listening on socket bound to: ~A:~A~%"
            (local-host server)
            (local-port server))
    (setf (acceptor-listen-socket acceptor)
	  server))

; names loop
(loop named outer for list in lists do
     (loop for item in list do
          (if (what-i-am-looking-for-p item)
            (return-from outer item))))

(defpackage "KEY-PARAM" ...)
(defpackage :key-param ...)
(defpackage #:key-param ...)
;here #: is a reader macro to create uninterned symbols; and this way is the preferred one, because you don't create unneeded keywords in the process.
