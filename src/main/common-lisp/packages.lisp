(in-package #:hunchentoot)

(export '(+new-connection-wait-time+))

(in-package #:common-lisp-user)

(defpackage #:iolib-acceptor
  (:use #:alexandria
        #:common-lisp)
  (:export #:iolib-acceptor-mixin))
