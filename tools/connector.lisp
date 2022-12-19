

(usocket:with-client-socket (sock stream "localhost" 8087 :element-type '(unsigned-byte 8))
(unwind-protect
     (progn
       (usocket:wait-for-input socket)
       (format t "Input is: ~a~%" (read-line stream)))
  (usocket:socket-close socket))))