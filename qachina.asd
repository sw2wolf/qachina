(asdf:defsystem #:qachina
  :serial t
  :depends-on (#:hunchentoot :hunchentoot-cgi
               #:cl-who
               #:cl-json
               ;#:parenscript
               #:css-lite)
  :components ((:file "package")
               (:file "qachina" :depends-on ("package"))))

