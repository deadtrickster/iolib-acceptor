(in-package #:iolib-acceptor)

(defclass iolib-acceptor (hunchentoot:acceptor)
  ())

;; Mixin HUNCHENTOOT:EASY-ACCEPTOR to use that dispatch framework.
(defclass iolib-easy-acceptor (hunchentoot:easy-acceptor iolib-acceptor)
  ())

#-:hunchentoot-no-ssl
(defclass iolib-ssl-acceptor (hunchentoot:ssl-acceptor iolib-acceptor)
  ())

#-:hunchentoot-no-ssl
(defclass iolib-easy-ssl-acceptor (hunchentoot:easy-acceptor iolib-ssl-acceptor)
  ())

(defmethod hunchentoot:stop ((acceptor iolib-acceptor) &key soft)
  (setf (hunchentoot::acceptor-shutdown-p acceptor) t)
  (shutdown (hunchentoot::acceptor-taskmaster acceptor))
  (when soft
    (bt:with-lock-held ((hunchentoot::acceptor-shutdown-lock acceptor))
      (when (plusp (hunchentoot::acceptor-requests-in-progress acceptor))
        (bt:condition-wait (hunchentoot::acceptor-shutdown-queue acceptor)
                           (hunchentoot::acceptor-shutdown-lock acceptor)))))
  (close (hunchentoot::acceptor-listen-socket acceptor))
  (setf (hunchentoot::acceptor-listen-socket acceptor) nil)
  acceptor)

(defmethod hunchentoot:start-listening ((acceptor iolib-acceptor))
  (when (hunchentoot::acceptor-listen-socket acceptor)
    (hunchentoot:hunchentoot-error "acceptor ~A is already listening" acceptor))
  (setf (hunchentoot::acceptor-listen-socket acceptor)
        (sockets:make-socket :connect :passive
                             :local-host (or (hunchentoot:acceptor-address acceptor)
                                             sockets:+ipv4-unspecified+)
                             :local-port (hunchentoot:acceptor-port acceptor)
                             :reuse-address t
                             :backlog (hunchentoot::acceptor-listen-backlog acceptor)))
  (values))

(defmethod hunchentoot:accept-connections ((acceptor iolib-acceptor))
  (let ((listener (hunchentoot::acceptor-listen-socket acceptor)))
    (unwind-protect
         (loop
            (when (hunchentoot::acceptor-shutdown-p acceptor)
              (return))
            (when-let (client-connection
                       (handler-case (sockets:accept-connection listener :wait t)
                         (sockets:socket-connection-aborted-error ())))
              (set-timeouts client-connection
                            (hunchentoot:acceptor-read-timeout acceptor)
                            (hunchentoot:acceptor-write-timeout acceptor))
              (hunchentoot:handle-incoming-connection
               (hunchentoot::acceptor-taskmaster acceptor) client-connection)))
      (close listener))))

(defun hunchentoot:client-as-string (socket)
  (multiple-value-bind (address port)
      (sockets:remote-name socket)
    (when (and address port)
      (format nil "~A:~A" (sockets:address-to-string address) port))))

(defun hunchentoot::get-peer-address-and-port (socket)
  (multiple-value-bind (address port)
      (sockets:remote-name socket)
    (values (sockets:address-to-string address) port)))

(defun hunchentoot::make-socket-stream (socket acceptor)
  (declare (ignore acceptor))
  socket)

(defun hunchentoot::read-initial-request-line (stream)
  (handler-case
      (let ((*current-error-message* "While reading initial request line:"))
        (with-mapped-conditions ()
          (read-line* stream)))
    ((or end-of-file sockets:socket-connection-timeout-error) ())))

(defun hunchentoot::set-timeouts (socket read-timeout write-timeout)
  (declare (ignorable socket read-timeout write-timeout))
  ;; add other Lisps here if necessary
  #+(or :sbcl :cmu)
  (unless (eql read-timeout write-timeout)
    (parameter-error "Read and write timeouts for socket must be equal."))
  #+:clisp
  (when read-timeout
    (socket:socket-options (sockets:socket-os-fd socket) :SO-RCVTIMEO read-timeout))
  #+:clisp
  (when write-timeout
    (socket:socket-options (sockets:socket-os-fd socket) :SO-SNDTIMEO write-timeout))
  #+:ecl
  (when read-timeout
    (print (list (sockets:socket-os-fd socket) read-timeout))
    (setf (sb-bsd-sockets:sockopt-receive-timeout (sockets:socket-os-fd socket))
	  read-timeout))
  #+:ecl
  (when write-timeout
    (print (list (sockets:socket-os-fd socket) write-timeout))
    (setf (sb-bsd-sockets:sockopt-send-timeout (sockets:socket-os-fd socket))
	  write-timeout))
  #+:openmcl
  (when read-timeout
    (setf (ccl:stream-input-timeout (sockets:socket-os-fd socket))
          read-timeout))
  #+:openmcl
  (when write-timeout
    (setf (ccl:stream-output-timeout (sockets:socket-os-fd socket))
          write-timeout))
  #+:sbcl
  (when read-timeout
    (setf (sb-impl::fd-stream-timeout socket)
          (coerce read-timeout 'single-float)))
  #+:cmu
  (setf (lisp::fd-stream-timeout socket)
        (coerce read-timeout 'integer))
  #-(or :clisp :allegro :openmcl :sbcl :lispworks :cmu :ecl)
  (not-implemented 'set-timeouts))

