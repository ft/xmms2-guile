;; Copyright (c) 2014 xmms2-guile workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (xmms2 io)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (web uri)
  #:use-module (xmms2 constants)
  #:use-module (xmms2 header)
  #:use-module (documentation more)
  #:export (make-xmms2-connection
            xmms2-connection?
            xmms2-connected?
            xmms2-ipv4?
            xmms2-ipv6?
            xmms2-unix?
            xmms2-connect
            xmms2-disconnect
            xmms2-recv
            xmms2-send
            xmms2-socket
            xmms2-uri))

(define protocol-tcp (protoent:proto (getprotobyname "tcp")))

(define-record-type <ip-connection>
  (make-ip-connection host address-string address port family)
  ip-connection?
  (family ipconn/get-family)
  (host ipconn/get-host)
  (address-string ipconn/get-address-string)
  (address ipconn/get-address)
  (port ipconn/get-port))

(define-record-type <unix-socket-connection>
  (make-unix-socket-connection file-name)
  unix-socket-connection?
  (file-name unix/get-file-name))

(define-record-type <xmms2-connection>
  (make-xmms2-connection* uri type state socket connection)
  xmms2-connection?
  (uri x2c/get-uri)
  (type x2c/get-type)
  (state x2c/get-state x2c/set-state!)
  (socket x2c/get-socket x2c/set-socket!)
  (connection x2c/get-connection))

(add-macro-docstring 'xmms2-connection?
  "Checks whether the datum ‘obj’ is a xmms2-connection container.")

(define-inlinable (xmms2/socket c)
  (x2c/get-socket c))

(define-inlinable (xmms2-uri c)
  (x2c/get-uri c))

(define (xmms2-connection-has-socket? conn)
  (if (x2c/get-socket conn) #t #f))

(define (xmms2-conn-family-test conn family)
  (and (xmms2-connection? conn)
       (let ((c (x2c/get-connection conn)))
         (and (ip-connection? c)
              (= (ipconn/get-family c) family)))))

(define (xmms2-ipv4? conn)
  (xmms2-conn-family-test conn AF_INET))

(define (xmms2-ipv6? conn)
  (xmms2-conn-family-test conn AF_INET6))

(define (xmms2-unix? conn)
  (and (xmms2-connection? conn)
       (eq? (x2c/get-type conn) 'unix)))

(define (safer-string->uri string)
  (let ((parsed (string->uri string)))
    (cond ((and parsed
                (not (uri-host parsed))
                (string? (uri-path parsed)))
           parsed)
          ((and parsed
                (uri-scheme parsed)
                (string? (uri-host parsed)))
           parsed)
          (else (let* ((next-try (string-concatenate (list "tcp://" string)))
                       (parsed (string->uri next-try)))
                  (if (and parsed (uri-host parsed))
                      parsed
                      (throw 'xmms2/invalid-uri string)))))))

(define (make-connection parsed-uri)
  (let* ((host (uri-host parsed-uri))
         (scheme (uri-scheme parsed-uri)))
    (if (eq? scheme 'unix)
        (let ((fname (uri-path parsed-uri)))
          (make-unix-socket-connection fname))
        (let* ((port (or (uri-port parsed-uri)
                         XMMS2-DEFAULT-PORT))
               (ai (car (catch #t
                          (lambda () (getaddrinfo host))
                          (lambda (key . args)
                            (throw 'xmms2/getaddrinfo-failed
                                   parsed-uri key args
                                   (gai-strerror (car args)))))))
               (family (addrinfo:fam ai))
               (addr (sockaddr:addr (addrinfo:addr ai)))
               (addr-string (catch #t
                              (lambda () (inet-ntop family addr))
                              (lambda (key . args)
                                (throw 'xmms2/inet-ntop-failed
                                       parsed-uri key args)))))
          (make-ip-connection host addr-string addr port family)))))

(define (make-unix-socket conn)
  (socket AF_UNIX SOCK_STREAM 0))

(define (make-tcp-socket conn)
  (socket (ipconn/get-family (x2c/get-connection conn))
          SOCK_STREAM protocol-tcp))

(define (disect-connect-error args)
  (match args
    ((_ _ (str) (num)) (list num str))
    (_ args)))

(define (connect-unix conn)
  (catch #t
    (lambda ()
      (connect (x2c/get-socket conn) AF_UNIX
               (unix/get-file-name (x2c/get-connection conn))))
    (lambda (key . args)
      (throw 'xmms2/connect-failed key (disect-connect-error args)))))

(define (connect-tcp conn)
  (let ((be (x2c/get-connection conn)))
    (catch #t
      (lambda ()
        (connect (x2c/get-socket conn)
                 (ipconn/get-family be)
                 (ipconn/get-address be)
                 (ipconn/get-port be)))
      (lambda (key . args)
        (throw 'xmms2/connect-failed key (disect-connect-error args))))))

(define (packet->data packet)
  (if (bytevector? packet)
      packet
      (let* ((l (map bytevector-length packet))
             (ls (apply + l))
             (data (make-bytevector ls )))
        (let loop ((in packet) (l l) (offset 0))
          (cond ((null? in) data)
                (else (let ((cur (car in))
                            (len (car l)))
                        (bytevector-copy! cur 0 data offset len)
                        (loop (cdr in) (cdr l) (+ offset len)))))))))

(define (make-xmms2-connection uri)
  "Create an object that encapsulates information for xmms2 server
connections.

‘uri’ is a string, that describes the location of the xmms2 server:

    scheme://location

Valid values for ‘scheme’ are: ‘unix’, ‘tcp’

In the case of the ‘unix’ scheme, LOCATION is an *absolute* file name. In case
of the ‘tcp’ scheme, it takes the form of: ~hostname-or-address[:port]~

If ‘scheme’ is missing from ‘uri’, ~tcp://~ is assumed.

The function may throw any of these exceptions:

  - ~xmms2/invalid-uri~ in case URI could not be parsed at all.

  - ~xmms2/invalid-uri-scheme~ in case the ~foo://~ part of URI is invalid.

  - ~xmms2/getaddrinfo-failed~ in case getaddrinfo failed.

  - ~xmms2/inet-ntop-failed~ in case inet-ntop failed."
  (define valid-schemes '(tcp unix))
  (let* ((parsed (safer-string->uri uri))
         (scheme (uri-scheme parsed)))
    (unless (memq scheme valid-schemes)
      (throw 'xmms2/invalid-uri-scheme uri scheme))
    (make-xmms2-connection* uri scheme 'disconnected
                            #f ;; socket is created when it's needed.
                            (make-connection parsed))))

(define (xmms2-connected? conn)
  "Predicate determining if ‘conn’ is in connected state."
  (if (eq? (x2c/get-state conn) 'connected) #t #f))

(define (xmms2-connect conn)
  "Connect to xmms2 server via ‘conn’.

Throws an exception ‘xmms2/already-connected’ in case the connection's state
yields to be connected already."
  (if (xmms2-connected? conn)
      (throw 'xmms2/already-connected conn)
      (begin (when (xmms2-connection-has-socket? conn)
               (shutdown (x2c/get-socket conn) 2)
               (x2c/set-socket! conn #f))
             (x2c/set-socket! conn (if (xmms2-unix? conn)
                                       (make-unix-socket conn)
                                       (make-tcp-socket conn)))
             (if (xmms2-unix? conn)
                 (connect-unix conn)
                 (connect-tcp conn))
             (x2c/set-state! conn 'connected))))

(define (xmms2-disconnect conn)
  "Disconnect server connection referenced by ‘conn’."
  (let ((s (x2c/get-socket conn)))
    (if (xmms2-connected? conn)
        (when s
          (shutdown s 2)
          ;; Return #t, since shutdown's return value is unspecified.
          #t)
        (and s (close-port s))))
  (x2c/set-state! conn 'disconnected)
  (x2c/set-socket! conn #f))

(define (xmms2-send conn packet)
  "Send ‘packet’ to xmms server via ‘conn’.

‘conn’ has to be an xmms2-connection. ‘conn’ has to be either a bytevector or a
list of multiple bytevectors, that will be send to the server in sequence.

Throws an exception ‘xmms2/send-failed’ in case send throws an exception, in
which case the connection referenced by ‘conn’ is also disconnected."
  (catch #t
    (lambda () (send (x2c/get-socket conn)
                     (packet->data packet)))
    (lambda (key . args)
      (xmms2-disconnect conn)
      (throw 'xmms2/send-failed conn key args))))

(define (xmms2-recv conn)
  "Perform a ~receive~ on an xmms2-connection

Reads a packet header, extracts the payload size from it and then goes on the
read that payload from the server connection referenced by ‘conn’. Returns a
list:

    (header payload-length payload)

This happens even if ‘payload-length’ is zero in which case ‘payload’ is an
empty bytevector.

Throws an exception ‘xmms2/recv-failed’ in case any of the procedures invoked
throw an exception, in which case the connection referenced by ‘conn’ is also
disconnected."
  (catch #t
    (lambda ()
      (let* ((header (get-bytevector-n (x2c/get-socket conn)
                                       XMMS2-HEADER-SIZE))
             (payload-length (header->payload-length header))
             (payload (if (zero? payload-length)
                          (make-bytevector 0)
                          (get-bytevector-n (x2c/get-socket conn)
                                            payload-length))))
        (list header payload-length payload)))
    (lambda (key . args)
      (xmms2-disconnect conn)
      (throw 'xmms2/recv-failed conn key args))))
