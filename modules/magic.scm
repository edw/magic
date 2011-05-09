;;;; Magic 3 -- a Web application server in Scheme (Scheme 48)
;;;; By Edwin Watkeys
;;;; 

;;; Character sets for parsing

(define whitespace '(#\space #\newline #\return #\tab))

;;; Query string and POST parameter decoding

(define (remove-empty-parameters lis)
  (filter (lambda (el) (> (string-length (cadr el)) 0))
          lis))

(define anything-but-equals-sign (char-set-complement (char-set #\=)))
(define anything-but-ampersand (char-set-complement (char-set #\&)))

(define (string->parameter-list s)
  (flatten (remove-empty-parameters
            (map (lambda (pair)
                   (key-string->symbol
                    (string-tokenize pair anything-but-equals-sign)))
                 (string-tokenize s anything-but-ampersand)))))

;;; Server

(define *server-socket* #f)
(define *last-request* #f)

(define (last-request-id)
  (and *last-request* (get-prop *last-request* 'thread-uid)))

(define (archive-request-maybe! request)
  (let ((path (get-prop request 'path)))
    (if (not (and path (pair? path) (eq? (car path) 'favicon.ico)))
        (set! *last-request* request))))

(define (symbolize s)
  (string->symbol (string-downcase s)))

(define (key-string->symbol l)
  (if (null? l) l
      (list (symbolize (url-decode (car l)))
            (url-decode (cadr-or-else l "")))))

(define (read-request-line in request)
  ((call-with-current-continuation 
    (lambda (k)
      (lambda ()
        (with-handler
         (lambda (c propagate)
           (k (lambda ()
                '(path (error-500)))))
         (lambda () (read-request-line-maybe-exploding in request))))))))

(define (read-request-line-maybe-exploding in request)
  (let* ((line (trim-back (read-line in) whitespace))
         (method+uri+protocol (tok line whitespace))
         (method (symbolize (car method+uri+protocol)))
         (uri (cadr method+uri+protocol))
         (protocol (caddr method+uri+protocol))
         (path+query-string (tok uri '(#\?)))
         (path (map (lambda (el) (symbolize el))
                    (tok (car path+query-string) '(#\/))))
         (query-string (if (null? (cdr path+query-string))
                           "" (cadr path+query-string)))
         (query-params (string->parameter-list query-string))
         (protocol-name+version (tok protocol '(#\/)))
         (protocol-name (car protocol-name+version))
         (protocol-major+minor-versions
          (tok (cadr protocol-name+version) '(#\.)))
         (protocol-major-version
          (string->number (car protocol-major+minor-versions)))
         (protocol-minor-version
          (string->number (cadr protocol-major+minor-versions))))
    (list 'method method
          'path path
          'query-parameters query-params
          'protocol-name protocol-name
          'protocol-major-version protocol-major-version
          'protocol-minor-version protocol-minor-version
          'thread-uid (thread-uid (current-thread))
          'time (time-seconds (current-time)))))

(define (read-headers in request)
  (let ((line (trim-back (read-line in) whitespace)))
    (if (zero? (string-length line))
        request
        (let ((name+value (snap line '(#\:))))
          (cond ((not name+value)
                 (note "garbage header line")
                 (read-headers in request))
                (else
                 (read-headers
                  in
                  (add-prop request
                            (symbolize (car name+value))
                            (trim-front (cadr name+value) whitespace)))))))))

(define (call-handler in out request)
  (let ((handler (find-handler request)))
    (call-with-current-continuation
     (lambda (k)
       (cond
        (handler
         (let-fluids
          $current-request request
          $current-response (make-cell '())
          $response-output-port out
          $response-sent? (make-cell #f)
          $response-escape-proc k
          (lambda () (handler)))
         (archive-request-maybe! request))
        (else
         (display "HTTP/1.1 404 Not Found" out)
         (newline out)
         (display "Content-type: text/plain" out)
         (newline out)
         (display "Server: Magic/3.0d0" out)
         (newline out)
         (newline out)
         (display "404 - Not Found" out)
         (newline out)
         (newline out)
         (write request out)
         (archive-request-maybe! request)
         ;; the previous line is a short-term debugging kludge
         (newline out)))))))

(define (process-request-content content request)
  (let ((parameters (string->parameter-list content)))
    (add-prop request 'post-parameters parameters)))

(define multipart-form-data-type "multipart/form-data")

(define (multipart-post? content-type method)
  (and (eq? method 'post)
       (starts-with? content-type multipart-form-data-type)))

(define post-content-type "application/x-www-form-urlencoded")
(define post-content-type-length (string-length post-content-type))

(define (urlencoded-post? content-type method)
  (let ((base-type
	 ;; What follows is a kludge. Should really split on a
	 ;; semi-colon. Should also re-write this to look for encoding
	 ;; type and interpret accordingly, or at the very least
	 ;; signal an error if the encoding is not UTF8.
	 (cond ((not content-type) "")
	       ((>= post-content-type-length (string-length content-type))
		content-type)
	       (else
		(if (char=? (string-ref content-type post-content-type-length)
			    #\;)
		    (substring content-type 0 post-content-type-length))))))
    (and (eq? method 'post)
	 (string=? base-type
		   post-content-type))))

(define-syntax time-me
  (syntax-rules ()
    ((time-me what e)
     (begin
       (display "beg " (current-error-port))
       (display what (current-error-port))
       (display " " (current-error-port))
       (display (current-time) (current-error-port))
       (newline (current-error-port))
       (let ((result e))
         (display "end " (current-error-port))
         (display what (current-error-port))
         (display " " (current-error-port))
         (display (current-time) (current-error-port))
         (newline (current-error-port))
         result)))))

(define (handle-urlencoded-post in request)
  (let* ((content-length
          (get-prop request 'content-length string->number))
         (bytes (make-byte-vector content-length 0))
         (count-read (read-block bytes 0 content-length in)))
    (cond ((eof-object? count-read)
           (server-error!
            "400 Bad Request"
            "<h1>Bad Request</h1><p>Ran out of POST data.</p>"))
          (else 
           (process-request-content
            (byte-vector->string utf-8-codec bytes)
            request)))))

(define (get-boundary content-type)
  (let ((boundary (trim-front (cadr (snap content-type '(#\;))) whitespace)))
    (if (starts-with? boundary "boundary=")
        (cadr (snap boundary '(#\=)))
        #f)))

(define (read-part-data-and-write-to-port in out boundary)
  (let ((seeker (make-seeker (string-append "\r\n--" boundary))))
    (let iter ((ch (seeker (read-char in) out)))
      (cond ((not ch)
             out)
            (else
             (iter (seeker (read-char in) out)))))))

(define (read-part-data in request boundary headers)
  (get-multipart-headers
   in
   (add-prop request
             'multipart-parameters
             (list (symbolize (get-prop headers 'name))
                   (string-output-port-output
                    (read-part-data-and-write-to-port
                     in (make-string-output-port)
                     boundary))))
   boundary))

(define (read-part-data-into-file in request boundary headers)
  (get-multipart-headers
   in
   (let ((file-name "temporary-file"))
     (call-with-output-file file-name
       (lambda (p)
         (read-part-data-and-write-to-port in p boundary)
         (add-prop request
                   'multipart-parameters
                   (list (symbolize (get-prop headers 'name))
                         (add-prop headers 'local-file-name file-name))))))
   boundary))

(define (get-file-name s)
  (let  ((parts (snap (trim-front s '(#\space #\;)) '(#\=))))
    (cond ((not parts)
           #f)
          ((not (string=? (car parts) "filename"))
           #f)
          (else
           (trim (cadr parts) '(#\"))))))

(define (get-part-name in request boundary headers dispo-info)
  (let ((name-key-snap (snap dispo-info '(#\=))))
    (cond ((not (and name-key-snap
                     (string=? (car name-key-snap) "name")))
           (note "key not 'name'")
           request)
          (else
           (let ((field-name-snap
                  (snap (trim-front (cadr name-key-snap) '(#\")) '(#\"))))
             (cond ((not field-name-snap)
                    (note "name value has no closing quote")
                    request)
                   (else
                    (let ((field-name (car field-name-snap))
                          (file-name (get-file-name (cadr field-name-snap))))
                      (if file-name
                          (read-part-data-into-file
                           in request boundary
                           (add-prop (add-prop headers 'original-file-name
                                               file-name)
                                     'name field-name))
                          (read-part-data
                           in request boundary
                           (add-prop dispo-info 'name field-name)))))))))))

(define (read-multipart-content in request boundary headers)
  (let ((dispo-header (get-prop headers 'content-disposition)))
    (cond ((not dispo-header)
           (note "no content-disposition header")
           request)
          (else
           (let ((dispo-parts (snap dispo-header '(#\;))))
             (cond ((not dispo-parts)
                    (note "no semicolon in content-disposition")
                    request)
                   ((not (string=? (car dispo-parts) "form-data"))
                    (note "content-dispoition not form-data")
                    request)
                   (else
                    (get-part-name in request boundary headers
                                   (trim (cadr dispo-parts)
                                         whitespace)))))))))

(define (cleanup-multipart-parameters request)
  (let iter ((request request) (params '()) (rest '()))
    (if (null? request)
        (add-prop (reverse rest) 'multipart-parameters params)
        (let ((key (car request))
              (value (cadr request))
              (request (cddr request)))
          (if (eq? key 'multipart-parameters)
              (let ((param-key (car value))
                    (param-value (cadr value)))
                (iter request
                      (add-prop params param-key param-value)
                      rest))
              (iter request
                    params
                    (cons value (cons key rest))))))))

(define (get-multipart-headers in request boundary)
  (case (dashes-crlf-or-false in)
    ((dashes)
     (cleanup-multipart-parameters request))
    ((crlf)
     (read-multipart-content in request boundary (read-headers in '())))
    (else
     (note "wtf?!")
     request)))

(define (dashes-crlf-or-false p)
  (case (read-char p)
    ((#\return)
     (case (read-char p)
       ((#\newline) 'crlf)
       (else #f)))
    ((#\-)
     (case (read-char p)
       ((#\-) 'dashes)
       (else #f)))
    (else #f)))

(define (get-multipart-boundary in request boundary)
  (cond ((not (eq? (dashes-crlf-or-false in) 'dashes))
         (note "didn't find dashes for boundary")
         request)
        (else
         (let ((boundary-max-index (- (string-length boundary) 1)))
           (let iter ((i 0))
             (let ((ch (read-char in)))
               (cond ((eof-object? ch)
                      request)
                     ((not (char=? ch (string-ref boundary i)))
                      (note "boundary not found")
                      request)
                     ((= i boundary-max-index)
                      (get-multipart-headers in request boundary))
                     (else
                      (iter (+ i 1))))))))))

(define (handle-multipart-post in request content-type)
  (let ((boundary (get-boundary content-type)))
    (get-multipart-boundary in request boundary)))

(define (read-request-content in request)
  (let ((content-type (get-prop request 'content-type))
        (method (get-prop request 'method)))
    (cond ((urlencoded-post? content-type method)
           (handle-urlencoded-post in request))
          ((multipart-post? content-type method)
           (handle-multipart-post in request content-type))
          (else request))))

(define (process-cookies request)
  (let ((query-params (get-prop request 'query-parameters))
        (cookie-params (get-cookie-parameters request)))
    (let ((has-query-params? (and query-params (not (null? query-params))))
          (has-cookie-params? (not (null? cookie-params))))
      (cond ((and has-query-params?
                  has-cookie-params?)
             (add-prop (add-prop (remove-prop request 'query-parameters)
                                 'cookie-parameters
                                 cookie-params)
                       'query-parameters
                       query-params))
            (has-cookie-params? ; therefore no query params...
             (add-prop request 'cookie-parameters cookie-params))
            (else ; therefore no cookie params...
             request)))))

(define (get-cookie-parameters request)
  (let ((cookies (get-prop request 'cookie)))
    (if (not cookies)
        '()
        (let ((pairs (tok cookies '(#\;))))
          (cond
           (pairs
            (flatten (fold
                      (lambda (a b)
                        (if a (cons (key-string->symbol a) b) b))
                      '()
                      (map (lambda (x) (tok
                                   (trim x whitespace) '(#\=)))
                           pairs))))
           (else '()))))))

(define (handle-request in out)
  (set-port-text-codec! in utf-8-codec)
  (set-port-text-codec! out utf-8-codec)
  (spawn
   (lambda ()
     (call-handler
      in out
      (read-request-content
       in (process-cookies (read-headers in (read-request-line in '())))))
     (close-input-port in)
     (close-output-port out))))

(define (request-param key request)
  (let iter ((request request))
    (if (null? request)
        #f
        (case (car request)
          ((post-parameters
            query-parameters
            multipart-parameters
            cookie-parameters)
           (let iter2 ((parameters (cadr request)))
             (cond ((null? parameters)
                    (iter (cddr request)))
                   ((eq? key (car parameters))
                    (cadr parameters))
                   (else (iter2 (cddr parameters))))))
          (else (iter (cddr request)))))))

(define (cleanup)
  (cond (*server-socket*
         (close-socket *server-socket*))
        (else
         (note "socket aleady closed"))))


(define (serve . port)
  (let ((port (if (null? port) 8081 (car port))))
    (if *server-socket*
        (note "socket already open")
        (set! *server-socket* (open-socket port)))
    (let iter ()
      (call-with-values (lambda () (socket-accept *server-socket*))
        handle-request)
      (iter))))

(define *handlers* (make-symbol-table))

(define (find-handler request)
  (let* ((path (get-prop request 'path))
         (handler-name
          (cond ((null? path) 'index)
                (else (car path)))))
    (table-ref *handlers* handler-name)))

(define (register-handler! name proc . max-age-maybe)
   (let ((max-age (and (not (null? max-age-maybe)) (car max-age-maybe))))
     (table-set! *handlers* name proc)
     (if max-age
         (call-later max-age (lambda () (table-set! *handlers* name #f))))))

(define (default d) (lambda (x) (or x d)))

(define (server-error! status message)
  (add-response-status! status)
  (send message)
  (escape-response))

(define (redirect! url)
  (add-response-status! "303 See other")
  (add-response-header! "Location" url)
  (send xml `(p "Moved to "
                ((a href ,url) ,url)
                ".")))

(define (send-response)
  (let ((response (fluid-cell-ref $current-response)))
    (let
        ((out (fluid $response-output-port))
         (status (get-prop response 'status
                           (default "200 OK")))
         (content-type (get-prop response 'content-type
                                 (default "text/html"))))
      (display "HTTP/1.1 " out)
      (display status out)
      (newline out)
      (display "Server: Magic/3" out)
      (newline out)
      (for-matching-props
       (lambda (cookie)
         (display "Set-cookie: " out)
         (display
          (make-cookie-string
           #f #f #f #f
           (symbol->string
            (car cookie)) (cadr cookie))
          out)
         (newline out))
       'cookie
       response)
      (for-matching-props
       (lambda (header)
         (display (car header) out)
         (display ": " out)
         (display (cadr header) out)
         (newline out))
       'header
       response)
      (display "Content-type: " out)
      (display content-type out)
      (newline out)
      (display "Connection: close" out)
      (newline out)
      (newline out)
      (fluid-cell-set! $response-sent? #t))))

(define $current-request (make-fluid #f))
(define $current-response (make-fluid #f))
(define $response-output-port (make-fluid #f))
(define $response-sent? (make-fluid #f))
(define $response-escape-proc (make-fluid #f))

;;; Procedures for request handlers

(define (escape-response)
  ((fluid $response-escape-proc)))

(define (current-request)
  (fluid $current-request))

(define (send object . rest)
  (or (fluid-cell-ref $response-sent?)
      (send-response))
  (if (procedure? object)
      (display (apply object rest) (fluid $response-output-port))
      (let ((proc (if (null? rest)
                      display
                      (car rest))))
        (proc object (fluid $response-output-port)))))

(define (send-newline)
  (send "\r"))

(define (add-response-header! k v)
  (add-response-prop! 'header (list k v)))

(define (add-response-prop! k v)
  (fluid-cell-set! $current-response
                   (add-prop (fluid-cell-ref $current-response) k v)))

(define (add-response-status! string)
  (add-response-prop! 'status string))

(define (add-response-cookie! k v)
  (add-response-prop! 'cookie (list k v)))

(define (add-response-content-type! mimetype)
  (add-response-prop! 'content-type mimetype))

(define (parameter key . rest)
  (let ((proc (if (null? rest)
                  (lambda (x) x)
                  (car rest))))
    (proc (request-param key (fluid $current-request)))))

(define-syntax define-handler
  (syntax-rules ()
    ((define-handler (name a1 ...) e1 e2 ...)
     (register-handler! 'name
                        (lambda ()
                          (let ((a1 (parameter 'a1))
                                ...)
                            e1 e2 ...))))))

(define click-handler-name-random-source (make-random-source))
(random-source-randomize! click-handler-name-random-source)
(define click-handler-name-random-integer
  (random-source-make-integers click-handler-name-random-source))

(define (make-click-handler-name)
  (string->symbol
   (string-append "click-"
                  (number->string (get-prop (current-request)
                                            'thread-uid))
                  "-"
                  (number->string (click-handler-name-random-integer
                                   100000000)))))

(define-syntax field
  (syntax-rules ()
    ((field field-name)
     '((input type text name field-name)))
    ((field field-name field-value)
     `((input type text name field-name value ,field-value)))))

(define-syntax file
  (syntax-rules ()
    ((file field-name)
     '((input type file name field-name)))))

(define-syntax submit
  (syntax-rules ()
    ((submit label)
     '((input type submit value label)))))

(define (make-ajax-click-snippet handler-name destination)
  (string-append "return magic_link('/"
                 (symbol->string handler-name)
                 "', '#"
                 (symbol->string destination)
                 "');"))

(define (make-ajax-form-snippet handler-name destination focus-url)
  (string-append "return magic_form(this, '/"
                 (symbol->string handler-name)
                 "', '#"
                 (symbol->string destination)
                 "', "
                 (if focus-url "true" "false")
                 ");"))

(define *click-handler-max-age* (* 60 60 1000)) ; Sixty minutes

(define-syntax ajax-link
  (syntax-rules ()
    ((ajax-link label destination expression)
     (let* ((handler-name (make-click-handler-name))
            (javascript (make-ajax-click-snippet handler-name destination)))
       (register-handler! handler-name
                          (lambda ()
                            (send xml expression)) *click-handler-max-age*)
       `((a href ,handler-name onclick ,javascript)
         ,label)))))

(define-syntax link
  (syntax-rules ()
    ((link label e1 e2 ...)
     (let ((handler-name (make-click-handler-name)))
       (register-handler! handler-name
                          (lambda ()
                            e1 e2 ...)
                          *click-handler-max-age*)
       `((a href ,(string-append "/" (symbol->string handler-name)))
         ,label)))))

(define (focus-url-maybe optionals)
  (if (null? optionals) #f
      (if (eq? 'focus (car optionals))
          (cadr optionals)
          #f)))

(define (ajax-stateless-link destination local-url label . optionals)
  (let ((focus-url (focus-url-maybe optionals)))
    `((a href ,(if focus-url focus-url local-url)
         onclick ,(make-ajax-form-snippet local-url destination focus-url))
      ,label)))

(define-syntax ajax-form
  (syntax-rules ()
    ((ajax-form destination (a1 ...) contents expression)
     (let* ((handler-name (make-click-handler-name))
            (javascript (make-ajax-form-snippet handler-name destination #f)))
       (register-handler! handler-name
                          (lambda ()
                            (let ((a1 (parameter 'a1))
                                  ...)
                              (send xml expression)))
                          *click-handler-max-age*)
       `((form onsubmit ,javascript)
         ,@contents)))))

(define-syntax form
  (syntax-rules (multi)
    ((form multi (a1 ...) contents e1 e2 ...)
     (form multipart/form-data (a1 ...) contents e1 e2 ...))
    ((form (a1 ...) contents e1 e2 ...)
     (form application/x-www-form-urlencoded (a1 ...) contents e1 e2 ...))
    ((form encoding-type (a1 ...) contents e1 e2 ...)
     (let ((handler-name (make-click-handler-name)))
       (register-handler! handler-name
                          (lambda ()
                            (let ((a1 (parameter 'a1))
                                  ...)
                              e1 e2 ...))
                          *click-handler-max-age*)
       `((form method post
               action ,(string-append "/" (symbol->string handler-name))
               enctype encoding-type)
         ,@contents)))))
