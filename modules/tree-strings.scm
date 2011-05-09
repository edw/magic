(define (string-copy! s i t) ; copy all of t into s at location i
  (let ((end (+ i (string-length t))))
    (let iter ((i i) (j 0))
      (cond ((= i end) (if #f #t))
            (else
             (string-set! s i (string-ref t j))
             (iter (+ i 1) (+ j 1)))))))

(define (tree-string-length tree)
  (tree-string-walk tree (lambda (atom i) (if #f #t))))

(define (tree-string->string tree)
  (let ((s (make-string (tree-string-length tree))))
    (tree-string-walk tree (lambda (atom i) (string-copy! s i atom)))
    s))

(define (tree-string-walk tree proc)
  (let iter ((tree tree) (sum 0))
    (cond ((null? tree) sum)
          ((null? (car tree)) (iter (cdr tree) sum))
          ((pair? (car tree))
                  (iter (cdr tree) (iter (car tree) sum)))
          (else 
           (proc (car tree) sum)
           (iter (cdr tree) (+ sum (string-length (car tree))))))))
