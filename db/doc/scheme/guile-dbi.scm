;下面是一个 SQLite3 的使用实例:
(use-modules (dbi dbi))
 
;; Log into the database.
(define db-obj (dbi-open "sqlite3" "my-example-db"))
 
;; Create a table.
(dbi-query db-obj "create table hellotable(id int, name varchar(15))")
 
;; Look at the return status of the last SQL command
(display db-obj) (newline)
 
;; Populate the table with values.
(dbi-query db-obj "insert into hellotable ('id', 'name') values('33', 'ola')")
(dbi-query db-obj "insert into hellotable ('id', 'name') values('34', 'dzien dobre')")
(dbi-query db-obj "insert into hellotable ('id', 'name') values('44', 'annyong haseyo')")
(display db-obj) (newline)
 
;; Display each of the rows of the table, in turn.
(dbi-query db-obj "select * from hellotable")
(display db-obj) (newline)
(write (dbi-get_row db-obj)) (newline)
(write (dbi-get_row db-obj)) (newline)
(write (dbi-get_row db-obj)) (newline)
(write (dbi-get_row db-obj)) (newline)
 
;; Close the database.
(dbi-close db-obj)
(display db-obj)(newline)


;下面是使用 MySQL 数据库的一个实例. 这个实例假定 MySQL 服务已经运行, 并且一个名为 pippo 的 table 已经被创建, 并被存储了一些数据:
#!/usr/bin/guile -e main -s
!#
 
(use-modules (dbi dbi))
  
(define ciccio (dbi-open "mysql" "user:pass:pluto:tcp:localhost:3306"))
(define ret #f)
;; (define ciccio (dbi-open "mysql" "user:pass:pluto:socket:/tmp/mysql.sock"))
 
(define main
  (lambda (args)
    (display "HERE")(newline)
    (display ciccio)(newline)
    (dbi-query ciccio "select * from pippo")
    (display ciccio)(newline)
    (set! ret (dbi-get_row ciccio))
    (let loop ()
         (if (not (equal? ret #f))
             (begin
              (display ret)(newline)
              (set! ret (dbi-get_row ciccio))
              (loop))))
    (display ret) (newline)))
