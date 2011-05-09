(define (hex->char a b)
  (integer->char (or (string->number (string a b) 16) 32)))

(define (url-encode s)
  (call-with-string-output-port
   (lambda (out)
     (let ((length (string-length s)))
      (let loop ((i 0))
        (if (= i length)
            (unspecific)
            (let ((ch (string-ref s i)))
              (cond ((or (char-alphabetic? ch)
                         (char-numeric? ch))
                     (write-char ch out)
                     (loop (+ i 1)))
                    (else
                     (write-char #\% out)
                     (let ((ch-code-string
                            (number->string (char->integer ch) 16)))
                       (if (= (string-length ch-code-string) 1)
                           (write-char #\0 out))
                       (write-string ch-code-string out))
                     (loop (+ i 1)))))))))))

(define (url-decode s)
  (let ((sl (string->list s)))
    (let loop ((sl sl) (tl '()))
      (if (null? sl)
          (reverse-list->string tl (length tl))
          (let ((ch (car sl))
                (sl (cdr sl)))
            (case ch
              ((#\+)
               (loop sl (cons #\space tl)))
              ((#\%)
               (cond ((null? sl)
                      (reverse-list->string tl (length tl)))
                     ((null? (cdr sl))
                      (reverse-list->string tl (length tl)))
                     (else
                      (loop (cddr sl)
                            (cons (hex->char (car sl) (cadr sl)) tl)))))
              (else
               (loop sl (cons ch tl)))))))))
