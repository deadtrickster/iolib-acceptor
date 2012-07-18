(asdf:defsystem #:iolib-acceptor
  :description "Trivially substitute IOLIB for USOCKET in HUNCHENTOOT."
  :license "MIT"
  :depends-on (#:hunchentoot
               #:iolib)
  :serial t
  :components
  ((:file "packages")
   (:module :src
            :components
            ((:file "acceptor")))))
