(define (starts-with? haystack needle)
  (let ((needle-length (string-length needle)))
    (if (< (string-length haystack) needle-length)
        #f
        (string=? (substring haystack 0 needle-length) needle))))

(define (string-downcase s)
  (list->string (map (lambda (ch) (char-downcase ch)) (string->list s))))

(define (byte-vector->string codec bytes)
  (bytes->string codec bytes #f))

(define (char-in-list? ch list)
  (let iter ((list list))
    (cond ((null? list) #f)
          ((char=? ch (car list))
           #t)
          (else
           (iter (cdr list))))))

(define (tok s seps)
  (let iter ((s (string->list s)) (cur '()) (cur-len 0) (toks '()))
    (cond ((and (null? s) (null? cur))
           (reverse toks))
          ((null? s)
           (reverse (cons (reverse-list->string cur cur-len) toks)))
          ((char-in-list? (car s) seps)
           (if (null? cur)
               (iter (cdr s) cur cur-len toks)
               (iter (cdr s) '() 0
                     (cons (reverse-list->string cur cur-len) toks))))
          (else (iter (cdr s) (cons (car s) cur) (+ cur-len 1) toks)))))

(define (make-seeker h)
  (let ((h-len (string-length h))
        (h-list (string->list h)))
    (let ((h-last-ch (string-ref h (- h-len 1)))
          (q (make-queue)))
      (lambda (ch p)
        (cond
         ((eof-object? ch)
          (for-each (lambda (ch) (write-char ch p)) (queue->list q))
          ch)
         (else
          (enqueue! q ch)
          (cond
           ((and (char=? ch h-last-ch)
                 (= (queue-length q) h-len)
                 (fold (lambda (a b seed)
                         (and seed (char=? a b)))
                       #t h-list (queue->list q)))
            #f)
           ((= (queue-length q) h-len)
            (write-char (dequeue! q) p)
            ch)
           (else
            ch))))))))

(define (snap s seps)
  (let ((s-length (string-length s)))
    (let iter ((i 0))
      (cond ((= i s-length) #f)
            ((char-in-list? (string-ref s i) seps)
             (list (substring s 0 i) (substring s (+ i 1) s-length)))
            (else (iter (+ i 1)))))))

(define (trim-back s victims)
  (let ((s-length (string-length s)))
    (let iter ((i (- s-length 1)))
      (cond ((< i 0) "")
            ((char-in-list? (string-ref s i) victims)
             (iter (- i 1)))
            (else
             (substring s 0 (+ i 1)))))))

(define (trim-front s victims)
  (let ((s-length (string-length s)))
   (let iter ((i 0))
     (cond ((= i s-length)
            s)
           ((char-in-list? (string-ref s i) victims)
            (iter (+ i 1)))
           (else
            (substring s i s-length))))))

(define (trim s victims)
  (trim-front (trim-back s victims) victims))

;;; List utilities

(define (flatten list)
  (if (null? list) list
      (apply append (car list) (cdr list))))
