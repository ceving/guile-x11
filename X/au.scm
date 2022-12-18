(define-module (X au)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:export (parse-xauthority
	    read-xauthority))

(define (u16)
  (let* ((msb (get-u8 port))
	 (lsb (get-u8 port)))
    (+ (* msb 256) lsb)))

(define (u8 n)
  (get-bytevector-n port n))

(define (parse-xauthority port)
  (let loop ((entries '()))
    (if (port-eof? port)
	entries
	(let* ((family (let ((family (u16)))
			 (case family
			   ((256) 'local)
			   ((65535) 'wild)
			   ((254) 'netname)
			   ((253) 'krb5principal)
			   ((252) 'localhost)
			   (else family))))
	       (address (let ((address (u8 (u16))))
			  (case family
			    ((local) (utf8->string address))
			    ((0) address) ;; maybe convert to number
			    (else address))))
	       (number (utf8->string (u8 (u16))))
	       (name (utf8->string (u8 (u16))))
	       (data (u8 (u16))))
	  (loop (cons (list (cons 'family family)
			    (cons 'address address)
			    (cons 'number number)
			    (cons 'name name)
			    (cons 'data data))
		      entries))))))

(define (read-xauthority . file-name)
  (let ((name (if (null? file-name)
		  (or (getenv "XAUTHORITY")
		      (string-append (getenv "HOME") "/.Xauthority"))
		  (car file-name))))
    (call-with-input-file name
      parse-xauthority
      #:guess-encoding #f
      #:binary #t)))
