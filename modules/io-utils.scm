(define (read-fully p)
  (let loop ((input '()) (len 0) (char (read-char p)))
    (if (eof-object? char)
        (reverse-list->string input len)
        (loop (cons char input) (+ len 1) (read-char p)))))

(define (read-line p)
  (let iter ((ch (read-char p)) (chs '()) (chs-count 0))
    (cond ((eof-object? ch)
           (if (null? chs)
               ch ; EOF object
               (reverse-list->string chs chs-count)))
          ((char=? ch #\newline) (reverse-list->string chs chs-count))
          (else (iter (read-char p) (cons ch chs) (+ chs-count 1))))))
