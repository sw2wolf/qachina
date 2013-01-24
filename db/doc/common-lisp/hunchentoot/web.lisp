; 一些辅助函数  
(require :asdf)  
(defun loadlib (mod)  
    (asdf:oos 'asdf:load-op mod))
    
(defun reload ()  
    (load "web.lisp"))

(defun restart-web ()  
    (progn  
        (reload)  
        (start-web)))
                  
; load 需要的库    
(loadlib :html-template)  
(loadlib :hunchentoot)  
  
; 设置 hunchentoot 编码  
(defvar *utf-8* (flex:make-external-format :utf-8 :eol-style :lf))  
(setq hunchentoot:*hunchentoot-default-external-format* *utf-8*)  

; 设置url handler 转发表  
(push (hunchentoot:create-prefix-dispatcher "/hello" 'hello) hunchentoot:*dispatch-table*)  
            
; 页面控制器函数  
(defun hello ()  
    (setf (hunchentoot:content-type*) "text/html; charset=utf-8")  
    (with-output-to-string (stream)  
        (html-template:fill-and-print-template  
            #p"index.tmpl"  
            (list :name "Lisp程序员")  
            :stream stream)))

; 启动服务器  
(defun start-web (&optional (port 8888))
    (hunchentoot:start (make-instance 'hunchentoot:acceptor :port port)))
