;;;; Magic 3 -- a Web application server in Scheme (Scheme 48)
;;;; By Edwin Watkeys
;;;; 

;;;; Sample code is at the bottom of the file. To start the server,
;;;; eval: (start "password")

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

(define (cadr-or-else pair else)
  (if (null? (cdr pair))
      else
      (cadr pair)))

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

;;; Sample code

;; General purpose request dumper that sets a cookie with k & v if
;; present.

(define-handler (dump k v)
  (add-response-content-type! "text/plain")
  (if (and k v) (add-response-cookie! (string->symbol k) v))
  (send (current-request) write)
  (send-newline))

;; A simple XHTML page template.

(define jquery-url "http://code.jquery.com/jquery-1.6.min.js")

(define (page title . body)
  (send "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
        \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
")
  (send xml `((html xmlns "http://www.w3.org/1999/xhtml"
                    xml:lang en
                    lang en)
              (head ((meta http-equiv "Content-Type"
                           content "text/html;charset=utf-8"))
                    (title ,title)
                    ,(js-include jquery-url)
                    ,(js-include "http://poseur.com/js/magic.js"))
              (body (div
                     ((img src "http://poseur.com/img/magic.png"
                           style "float: right" alt "magic logo"))
                     (h1 ,title))
                    ,@body))))

;; Acid test

(define-handler (acid-test)
  (send xml
   `((form action dump
           method post
           enctype multipart/form-data)
     f: ,(file f) (br)
     x: ,(field x 42) (br)
     y: ,(field y 1) (br)
     g: ,(file g) (br)
     z: ,(field z 43) (br)
     h: ,(file h) (br)
     ,(submit "Go"))))

;; Redirect test

(define-handler (redirect)
  (redirect! "http://google.com/"))

;; Front page

(define-handler (index)
  (page
   "Magic 3: A web framework for Scheme 48"
   '(p "Magic 3 is a framework for writing web applications."
       " It meets the "
       ((a href "http://www.paulgraham.com/arcchallenge.html")
        "Arc Challenge")
       " quite nicely, thank you.")
   '(pre
     (code
      "(define-handler (" ((a href said) "said") ")" (br)
      "  (page" (br)
      "   \"Arc Challenge\"" (br)
      "   (ajax-form 'output (message)" (br)
      "              `(,(field message) ,(submit \"Go\"))" (br)
      "              `(p" (br)
      "                ,(ajax-link \"Click here\" 'output" (br)
      "                            `(p \"You said: \" ,message))))" (br)
      "   '((div id output))))" (br)
      ))
  '(p "The " ((a href "http://github.com/edw/magic/")
	      "source code")
      " is WTFPL-licensed and is free of external dependencies on any other"
      " packages, so you can get up and running quickly. Have fun.")))

;; Train schedule

(define (ul . items)
  `(ul ,@(map (lambda (item) `(li ,@(if (list? item)
                                        item
                                        (list item))))
              items)))

;; (define (js . lines)
;;   `((script type "text/javascript")
;;     ,@(fold (lambda (a b) ()))))

(define (js-include url)
  `((script type "text/javascript" src ,url) ""))

(define (css-include url)
  `((link rel "stylesheet" type "text/css" href ,url)))

(define *tz-adjustment* (*  3 60 60))

(define (local-time)
  (let ((s (time->string (make-time (+ *tz-adjustment* (time-seconds (current-time)))))))
    (map string->number (list
      (substring s 11 13)
      (substring s 14 16)))))

(define (format-local-time lt)
  (string-append (number->string (car lt))
                 ":"
                 (if (> (cadr lt) 9)
                     ""
                     "0")
                 (number->string (cadr lt))))

(define (format-minutes m)
  (let ((h (quotient m 60))
        (m (remainder m 60)))
    (string-append
     (number->string h)
     ":"
     (if (> m 9) "" "0")
     (number->string m))))

(define (local-time-difference smaller bigger)
  (+ (* 60 (- (car bigger) (car smaller)))
     (- (cadr bigger) (cadr smaller))))

(define sched
  (list '("Market East to Norristown" .
          ((7 07) (7 40) (8 08) (8 56) (9 45) (10 30) (11 30)))
        '("Conshohocken to Philadelphia" .
          ((15 43) (16 48) (17 15) (17 53) (18 21) (19 15) (20 00)))))

(define (sched->xml)
  (let ((lt (local-time)))
    (define (process-sched-time t)
      (let ((diff (local-time-difference lt t)))
        (cond ((> diff 0)
               (list
                (format-local-time t)
                " ("
                (format-minutes diff)
                ")"))
              (else '()))))
    (cons
     'dl
     (flatten (map
               (lambda (e) `((dt ,(car e)) (dd (ul ,@(cdr e)))))
               (filter (lambda (e) (> (length e) 1))
                       (map (lambda (e)
                              (cons (car e)
                                    (map
                                     (lambda (e) `(li ,@e))
                                     (filter
                                      (lambda (e) (not (null? e)))
                                      (map process-sched-time (cdr e))))))
                            sched)))))))

(define-handler (r6)
  (send (xml `(html (head (title "R6 Schedule"))
                (body ,(sched->xml))))))

;; Wiki

(define (raw-wiki-page page-name)
  (add-response-content-type! "text/plain")
  (send (read-file page-name)))

(define (wiki-page page-name)
  (let ((body (markdown-file page-name)))
    (send "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
        \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
")
    (send xml `((html xmlns "http://www.w3.org/1999/xhtml"
                      xml:lang en
                      lang en)
                (head ((meta http-equiv "Content-Type"
                             content "text/html;charset=utf-8"))
                      (title "Wiki: ",page-name)
                      ,(css-include "/wraw/wiki-css")
                      ,(js-include jquery-url)
                      ,(js-include "http://poseur.com/js/magic.js"))
                (body ((div id "wikictls")
                       (p "This is a wiki page. "
                          ,(ajax-stateless-link
                            'editdiv
                            (string->symbol (string-append "weditinplace/"
                                                           page-name))
                            "Edit It." 'focus (string->symbol "#editdiv"))
                          " Or consider going to the "
                          ((a href "/wiki/") "front page")
                          " or a list of "
                          ((a href "/wpages") "every page")
                          "."))
                      ,@body
                      ((div id "editdiv")))))))

(define *evil-path-name* "front")
(define *wiki-dir* "var/wiki/")

(define *wiki-link-re* (make-regexp "\\{\\{([a-zA-Z-]+)\\}\\}" 
                                   (regexp-option submatches)
                                   (regexp-option extended)))

(define (replace-wiki-links content)
  (define (make-wiki-link match)
    (let ((page-name
           (substring content (match-start match) (match-end match))))
      (string-append "[" page-name "](/wiki/" page-name ")")))
  (let loop ((i 0) (s ""))
    (let ((matches (regexp-match *wiki-link-re* content i #t #f #f)))
      (if matches
          (loop (match-end (car matches))
                (string-append
                 s
                 (substring content i (match-start (car matches)))
                 (make-wiki-link (cadr matches))))
          (string-append s (substring content i (string-length content)))))))

(define (tainted-path? path)
  (or (not path) (not (not (string-contains path "../")))))

(define (write-wiki-file path-name content)
  (if (not (tainted-path? path-name))
      (call-with-output-file (string-append *wiki-dir* path-name)
        (lambda (port)
          (write-string content port)))))

(define (read-file path-name)
  (let ((path-name (string-append *wiki-dir* path-name)))
    (if (accessible? path-name (access-mode exists))
        (call-with-input-file
            path-name
            (lambda (port)
              (set-port-text-codec! port utf-8-codec)
              (read-fully port)))
        #f)))

(define (markdown t)
  (filter-chain utf-8-codec
		t "var/wiki/bin/Markdown.pl" "var/wiki/bin/SmartyPants.pl"))

(define (markdown-file page-name)
  (let ((file-contents (markdown (replace-wiki-links (or (read-file page-name)
                                      "# Not Found

Sorry, we couldn't find anything by that name.")))))
    `(,(literally file-contents))))

(define-handler (wiki p)
  (let ((p (or p
                (symbol->string (cadr-or-else 
                                 (get-prop (current-request) 'path)
                                 'front)))))
    (if (tainted-path? p)
        (redirect! (string-append "/wiki/" *evil-path-name*))
        (wiki-page p))))

(define *wiki-pw* "password")

(define-handler (wraw p)
  (let ((p (or p
               (symbol->string (cadr-or-else
                                (get-prop (current-request) 'path)
                                'front)))))
    (if (tainted-path? p)
        (redirect! (string-append "/wraw/" *evil-path-name*))
        (raw-wiki-page p))))

(define-handler (wedit p pw)
  (define (get-path)
    (cond (p
           (if (tainted-path? p)
               'front
               p))
          ((cadr-or-else (get-prop (current-request) 'path) #f)
           => symbol->string)
          (else 'front)))
  (let* ((p (get-path))
         (page-content (read-file p)))
    (page "Edit Page"
          `(h2 "Page name: "
               ,p
               ,@(if (not page-content)
                     '(" (this will be a new page)")
                     '()))
          (form (page-content pw)
                `(((textarea rows 20 cols 80 name "page-content")
                   ,(if (not page-content) "" page-content))
                  (p ((input type submit value "Save"))
                     ((input type password name pw value ,(or pw "")))))
                (cond ((and (string? pw) (string=? pw *wiki-pw*))
                       (write-wiki-file p page-content)
                       (add-response-cookie! 'pw pw)
                       (redirect! (string-append "/wiki/" p)))
                      (else
                       (page "Wrong Password" '(p "Try again."))))))))
(define-handler (weditinplace p pw)
  (define (get-path)
    (cond (p
           (if (tainted-path? p)
               'front
               p))
          ((cadr-or-else (get-prop (current-request) 'path) #f)
           => symbol->string)
          (else 'front)))
  (let* ((p (get-path))
         (page-content (read-file p)))
    (send xml
          '(hr)
          (if (not page-content)
                '(h2 " (this will be a new page)")
                "")
          ;; Write a STATELESS-AJAX-FORM with key-value pairs to
          ;; submit so we can avoid having to time out. (Hmm, this Arc
          ;; Challenge stuff causes garbage to collect on the server
          ;; and we do everything we can to avoid using it... Hmm...)
          (form (page-content pw)
                `(((textarea rows 20 cols 80 name "page-content")
                   ,(if (not page-content) "" page-content))
                  (p ((input type submit value "Save"))
                     ((input type password name pw value ,(or pw "")))))
                (cond ((and (string? pw) (string=? pw *wiki-pw*))
                       (write-wiki-file p page-content)
                       (add-response-cookie! 'pw pw)
                       (redirect! (string-append "/wiki/" p)))
                      (else
                       (page "Wrong Password" '(p "Try again."))))))))

(define (list-regular-files dirname)
  (define (regular-file? filename)
    (let ((finfo (get-file-info (string-append *wiki-dir* filename))))
      (eq? (file-info-type finfo) (file-type regular))))
  (let loop ((dir (open-directory-stream dirname))
             (files '()))
    (let ((el (read-directory-stream dir)))
      (cond ((not el)
             (close-directory-stream dir)
             (reverse files))
            (el
             => (lambda (el)
                  (let ((filename (os-string->string el)))
                    (loop dir
                          (if (regular-file? filename)
                              (cons filename files)
                              files)))))))))

(define-handler (wpages)
  (page "Wiki Pages"
        `(ul ,@(map (lambda (el)
                      `(li ((a href ,(string-append "/wiki/" el)) ,el)))
                    (list-regular-files *wiki-dir*)))))

(define (start pw)
  (spawn minder)
  (spawn serve)
  (set! *wiki-pw* pw))

(define-handler (said)
  (page
   "Arc Challenge"
   (ajax-form 'output (message)
              `(,(field message) ,(submit "Go"))
              `(p
                ,(ajax-link "Click here" 'output
                            `(p "You said: " ,message))))
   '((div id output))))

;; End of file
