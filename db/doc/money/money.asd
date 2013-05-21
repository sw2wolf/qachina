(asdf:defsystem #:money
  :serial t ;ASDF will add dependencies for each child component, on all the children textually preceding it.
            ;This is done as if by :depends-on. so money will depends on package
  ;:depends-on (#:cl)
  ;:depends-on (#:optima)
  :components ((:file "package")
               (:file "money")))

