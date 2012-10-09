(asdf:defsystem #:iolib-acceptor
  :description "Trivially substitute IOLIB for USOCKET in HUNCHENTOOT."
  :license "MIT"
  :depends-on (#:hunchentoot
               #:iolib)
  :components
  ((:module :src
            :pathname "src/main/common-lisp"
            :serial t
            :components
            ((:file "packages")
             (:file "acceptor")))))
