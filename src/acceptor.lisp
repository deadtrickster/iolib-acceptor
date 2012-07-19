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
                       (handler-case (sockets:accept-connection listener :wait hunchentoot::+new-connection-wait-time+)
                         (sockets:socket-connection-aborted-error ())))
              (hunchentoot::set-timeouts client-connection
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
  (setf (sockets:socket-option socket :receive-timeout) read-timeout
        (sockets:socket-option socket :send-timeout) write-timeout))

(defun usocket::handle-condition (condition &optional socket)
  (declare (ignore socket))
  (typecase condition
    (serious-condition (error condition))
    (condition         (signal condition))))
