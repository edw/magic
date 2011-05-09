(define (add-prop plist name value)
  (cons name (cons value plist)))

(define (get-prop plist name . rest)
  (let ((proc (if (null? rest) (lambda (x) x) (car rest))))
    (proc (let iter ((plist plist))
            (cond ((null? plist) #f)
                  ((eq? (car plist) name)
                   (cadr plist))
                  (else (iter (cddr plist))))))))

(define (remove-prop plist key)
  (let iter ((plist plist) (new '()))
    (cond ((null? plist) (reverse new))
          ((eq? (car plist) key) (append (reverse new) (cddr plist)))
          (else (iter (cddr plist)
                      (cons (cadr plist) (cons (car plist) new)))))))

(define (for-matching-props proc key plist)
  (let iter ((plist plist))
    (cond ((null? plist)
           (if #f #t))
          ((eq? key (car plist))
           (proc (cadr plist))
           (iter (cddr plist)))
          (else
           (iter (cddr plist))))))
