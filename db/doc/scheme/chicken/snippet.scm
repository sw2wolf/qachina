(use posix srfi-18)

(define t
    (make-thread
        (lambda ()
            (process-run "csi -s i-want-to-keep-running.scm")
            (sleep 10))))

(thread-start! t)
(thread-join! t)
