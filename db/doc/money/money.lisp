(defpackage #:money
    (:nicknames #:m)
    (:use :cl)
    (:export :win-ssq :hit-ssq :his :sh
             :winG :stopLoss :div618))

(in-package :money)

(defparameter *base-dir* (directory-namestring *load-truename*))

(export '(*base-dir*
          pkg-src-dir pkg-ver find-pkg sys-info
          fac fab perfect-number bits
          sh sd range now leap-year-p assoc* combLen))

#|
   Utility
|#
;;; time relative
(defun leap-year-p (year)
    (destructuring-bind (fh h f)
        (mapcar #'(lambda (n) (zerop (mod year n))) '(400 100 4))
            (or fh (and (not h) f))))

(defun now ()
    (multiple-value-bind (second minute hour day month year) (get-decoded-time)
        (format t "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" year month day hour minute second)))

;;; OS relative
(defun sh (cmd)
    #+clisp
    (let ((str (ext:run-shell-command cmd :output :stream :wait nil)))
        (loop for line = (read-line str nil)
            until (null line)
            do (print line)))
    #+ecl (si:system cmd)
    #+sbcl (sb-ext:run-program "/bin/sh" (list "-c" cmd) :input nil :output *standard-output*)
    #+clozure (ccl:run-program "/bin/sh" (list "-c" cmd) :input nil :output *standard-output*))

;; (defun my-getenv (name &optional default)
;; #+CMU
;;     (let ((x (assoc name ext:*environment-list* :test #'string=)))
;;         (if x (cdr x) default))
;; #-CMU
;;     (or
;;         #+Allegro (sys:getenv name)
;;         #+CLISP (ext:getenv name)
;;         #+ECL (si:getenv name)
;;         #+SBCL (sb-unix::posix-getenv name)
;;         #+LISPWORKS (lispworks:environment-variable name)
;;         default))

(defun sys-info ()
    (format t "Machine: ~S ~S ~S~%OS: ~S ~S~%Lisp: ~S ~S~%"
        (machine-type) (machine-version) (machine-instance)
        (software-type) (software-version)
        (lisp-implementation-type) (lisp-implementation-version )))

;; (defun pkg-src-dir (name) 
;;      (asdf:system-source-directory name))
 
;; (defun pkg-ver (system-designator)
;;    (let ((system (asdf:find-system system-designator nil)))
;;        (when (and system (slot-boundp system 'asdf:version))
;;              (asdf:component-version system))))
 
;; (defun find-pkg (name)
;;    (ql:system-apropos name))

;; (declaim (inline whitespacep))
;; (defun whitespacep (c)
;;   "Checks whether C is a whitespace character."
;;   (find c '(#\Space #\Tab #\Newline #\Linefeed #\Return #\Page)))

;;; misc
(defgeneric assoc* (thing alist)
    (:documentation "Similar to CL:ASSOC, but 'does the right thing' if
THING is a string or a symbol.")
    (:method ((thing symbol) alist)
        (assoc thing alist :test #'eq))
    (:method ((thing string) alist)
        (assoc thing alist :test #'string-equal))
    (:method (thing alist)
        (assoc thing alist :test #'eql)))

;(defun qachina ()
;    (let ((cmd "cd /media/D/qachina; ./start.bat"))
;        #+ecl (mp:process-run-function 'qachina #'(lambda () (si:system cmd)))
;        #+clisp (mt:make-thread #'(lambda () (ext:shell cmd)))
;        #+ccl (process-run-function "qachina" #'(lambda () (sh cmd)))
;        #-(or ecl ccl clisp) (sh cmd)))

(defun sd (word) (sh (concatenate 'string "sdcv -n " word)))

(defun fac (n) (reduce #'* (loop for i from 1 to n  collect i)))

(defun fab (n) 
    (let ((res (list 1 1)))
        (loop for i from 2 to n do
            (nconc res (list (+ (nth (- i 2) res) (nth (- i 1) res)))))
        res))

(defun hex (value &optional (size 4))
  (format t "~v,'0X" size value))

(defun bits (value &optional (size 8))
  (format t "~v,'0B" size value))

(defun perfectp (n)
  (= n (loop for i from 1 below n when (= 0 (mod n i)) sum i)))

(defun perfect-numbers (s e) 
"Perfect Number"
    (loop for i from s to e 
        when (perfectp i) collect i))

(defun combLen (m n)
  (assert (>= m n) (n) "m >= n")
  (let ((X) (Y))
	(setf X (loop for total = 1 then (* total i)
			      for i from (+ 1 (- m n)) to m finally (return total)))
	(setf Y (loop for total = 1 then (* total i)
			      for i from 1 to n finally (return total)))
	(format t "It is ~d~%" (/ X Y))))

#|
   Stock Exchange
|#
(defconstant SXF 0.0015) ;手续费
(defconstant YHS 0.001)  ;印花费
(defconstant GHF 1.0)    ;过户费

(defun winG (qty pb ps)
"算股票盈利"
  (format t "You win: ~$~%"
		  (- (* qty ps (- 1 SXF YHS)) (* 2 GHF) (* qty pb (+ 1 SXF)))))

(defun winQ (qty pb ps)
"算权证盈利"
  (format t "You win: ~$~%"
		  (- (* qty ps (- 1 SXF)) (* 2 GHF) (* qty pb (+ 1 SXF)))))

(defun stopLoss (qty pb &optional (lossRate 0.03))
"止损价"
  (let ((tot (* qty pb (+ 1 SXF))))
        (format t "Stop Loss at:~$~%" (- pb (/ (* tot lossRate) qty)))
        (format t "Lost Money:~$(~d%)~%" (* tot lossRate) (* 100 lossRate))))

(defun div618 (p1 p2)
"黄金分割"
    (let ((ratio '(0. 0.191 0.236 0.382 0.5 0.618 0.809 1.))
          (price #'(lambda (r)
					   (if (<= p1 p2)
						   (+ p1 (* (- p2 p1) r))
						   (- p1 (* (- p1 p2) r))))))
       (if (<= p1 p2)
         (dolist (r (reverse ratio)) (format t "-------~3$   ~$-------~%" r (funcall price r)))
         (dolist (r ratio) (format t "-------~3$  ~$-------~%" r (funcall price r))))))

#|
   Lottery
|#
(defun range (n &optional (step 1)) (loop :for x :from 1 :to n :by step :collect x))

(defun str2lst (str) (with-input-from-string (s str) (loop :for x = (read s nil :end) :until (eq x :end) :collect x)))

(defun lst2str (lst)
";For each item in the input list (~{), print the item by its normal conversion (~S).
If no items remain, stop the iteration (~^). Otherwise, print a space, and then repeat
the process with the next item (~})."
    (format nil "~{~S ~^~}" lst))

(defun pick-num (from nums)
    (let ((res '()) (n))
        (dotimes (i nums) 
            (setq n (nth (random (length from)) from))
            (setq from (remove n from))
			(sleep 0.1)
            (push n res))
    res))

;; (defparameter +ssq-hit-num+ (namestring (asdf:system-relative-pathname 'money "ssqHitNum.txt")))
;; (defparameter +ssq-num+ (namestring (asdf:system-relative-pathname 'money "ssqNum.txt")))
(defparameter +ssq-hit-num+ "/media/D/qachina/db/doc/money/ssqHitNum.txt")
(defparameter +ssq-num+ "/media/D/qachina/db/doc/money/ssqNum.txt")

(defun good-red ()
    (let ((tab (make-hash-table)) (res '()) (nums) (sort-res))
        (dotimes (i 33) (setf (gethash (+ i 1) tab) 0))
        (with-open-file (stream +ssq-hit-num+)
            (loop :for line = (read-line stream nil)
                 :until (null line)
                 :do
                    (setq nums (butlast (str2lst (subseq line 6))))
                    (dolist (n nums) (incf (gethash n tab)))
                    ))
        (maphash #'(lambda (k v) (push (cons k v) res)) tab)
        (setq sort-res (sort res #'> :key #'cdr))
        (sort (subseq (mapcar #'car sort-res) 0 21) #'<)))

(defun win-ssq (nums no-red no-blue)
    (let* ((res) (resRed) (no-red-lst (str2lst no-red))
           ;(yesGRed (good-red)) ;(set-difference (good-red) no-red-lst))
		   (yesRed (set-difference (range 33) no-red-lst))
           (okBlue (pick-num (set-difference (range 16) (str2lst no-blue)) nums)))
        (assert (>= nums 1) (nums) "注数必须>=1")
        (setf *random-state* (make-random-state t))
        (with-open-file (out +ssq-num+ :direction :output :if-exists :supersede)
            (dotimes (i nums)
                (setf resRed (sort
				    ;; (if (= i (1- nums))
					;; 	(pick-num yesGRed 6)
					;; 	(pick-num yesRed 6))
				  (pick-num yesRed 6) #'>))
                (setf res (lst2str (reverse (cons (nth i okBlue) resRed))))
                (write-line res out)
                (format t "~A~%" res)))))

(defun hit-desc (red blue)
    (let ((res "X"))
    (cond
        ((and (= red 6) (= blue 1)) (setf res "First"))
        ((and (= red 6) (= blue 0)) (setf res "Second"))
        ((and (= red 5) (= blue 1)) (setf res "Third(3000)"))
        ((or (and (= red 5) (= blue 0)) (and (= red 4) (= blue 1))) (setf res "Fourth(200)"))
        ((or (and (= red 4) (= blue 0)) (and (= red 3) (= blue 1))) (setf res "Fifth(10)"))
        ((= blue 1) (setf res "Sixth(5)")))
    res))

(defun his () (sh (concatenate 'string "tail " +ssq-hit-num+)))

(defun hitnum-saved? (term) 
    (let ((ret nil))
        (with-open-file (f +ssq-hit-num+ :direction :input)
            (loop :for line = (read-line f nil)
                :until (or ret (null line))
                :do (setf ret (string= term (subseq line 0 5)))
                ;:finally (close f)
                ))
        ret))

(defun save-hitnum (term hitNum)
    (with-open-file (f +ssq-hit-num+ :direction :output :if-does-not-exist :create :if-exists :append)
        (write-line (concatenate 'string term " " hitNum) f)))

(defun hit-ssq (term hitNum)
    (let ((hit-num (str2lst hitNum)) (hit-red 0) (hit-blue 0) (num))
        (when (not (hitnum-saved? term)) (save-hitnum term hitNum))
        (format t "Good Red Hit:~D~%" (length (intersection (butlast hit-num) (butlast (good-red)))))
        (with-open-file (f +ssq-num+ :direction :input)
            (loop :for line = (read-line f nil)
                 :until (null line)
                 :do
                    (setf num (str2lst line))
                    (setf hit-red (length (intersection (butlast num) (butlast hit-num) )))
                    (if (= (seventh hit-num) (seventh num)) (setf hit-blue 1) (setf hit-blue 0))
                    (format t "~22a~10t(~d ~d)   ~a~%" line hit-red hit-blue (hit-desc hit-red hit-blue))))))
