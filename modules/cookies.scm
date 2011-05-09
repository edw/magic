(define (secure-clause secure)
  (cond (secure " secure")
        (else "")))

(define (path-clause path)
  (cond (path (list " path=" path ";"))
        (else "")))

(define (domain-clause domain)
  (cond (domain (list " domain=" domain ";"))
        (else "")))

(define (time->cookie-date-string time)
  (let ((s (time->string time)))
    (list (substring s 0 3)
          ", "
          (let ((s (substring s 8 10)))
            (if (char=? (string-ref s 0)
                        #\space)
                (string-set! s 0 #\0))
            s)
          "-"
          (substring s 4 7)
          "-"
          (substring s 20 24)
          " "
          (let ((s (substring s 11 19)))
            (if (char=? (string-ref s 0)
                        #\space)
                (string-set! s 0 #\0))
            s)
          " GMT")))

(define (expires-clause expires)
  (cond ((not expires)
         (expires-clause (* 86400 365 10)))
        ((number? expires)
         (list
          " expires="
          (time->cookie-date-string
           (make-time (+ (time-seconds (current-time))
                         expires)))
          ";"))
        (else "")))

(define (make-cookie-string expires path domain secure k v)
  (make-cookie-string-unescaped expires
                                path
                                domain
                                secure
                                (url-encode k)
                                (url-encode v)))

(define (make-cookie-string-unescaped expires path domain secure k v)
  (tree-string->string
   (list k "=" v ";"
         (expires-clause expires)
         (path-clause path)
         (domain-clause domain)
         (secure-clause secure))))

(define split-cookie-pairs
  (let ((cookie-seperators '(#\= #\; #\space #\tab)))
    (lambda (s)
      (let ((s-length (string-length s)))
        (let loop ((a 0)
                   (b 0))
          (cond ((= a b)
                 (cond ((= b s-length)
                        '())
                       ((char-in-list? (string-ref s b) cookie-seperators)
                        (loop (+ a 1)
                              (+ b 1)))
                       (else (loop a (+ b 1)))))
                (else
                 (cond ((= b s-length)
                        (cons (substring s a b)
                              '()))
                       ((char-in-list?  (string-ref s b) cookie-seperators)
                        (cons (substring s a b)
                              (loop (+ b 1)
                                    (+ b 1))))
                       (else (loop a (+ b 1)))))))))))
