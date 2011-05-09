(define (entitize s)
  (call-with-string-output-port
   (lambda (out)
     (set-port-text-codec! out utf-8-codec)
     (let ((length (string-length s)))
       (let loop ((i 0))
         (if (= i length)
             (unspecific)
             (let ((ch (string-ref s i)))
               (case ch
                 ((#\>) (write-string "&gt;" out))
                 ((#\<) (write-string "&lt;" out))
                 ((#\") (write-string "&quot;" out))
                 ((#\&) (write-string "&amp;" out))
                 (else
                  (write-char ch out)))
               (loop (+ i 1)))))))))

(define (render-attr-name attrs s)
  (cond ((null? attrs) 
	 s)
        (else
         (render-attr-value (cdr attrs)
                            (list s
                                  " "
                                  (symbol->string (car attrs))
                                  "=\"")))))

(define (render-attr-value attrs s)
  (cond ((null? attrs) (list s "\""))
	(else
         (let* ((attr-value (car attrs))
                (attr-value
                 (cond ((string? attr-value) attr-value)
                       ((symbol? attr-value) (symbol->string attr-value))
                       ((number? attr-value) (number->string attr-value)))))
           (render-attr-name
            (cdr attrs)
            (list s (entitize attr-value) "\""))))))

(define (render-name&attrs name&attrs)
  (cond ((pair? name&attrs)
	 (list (symbol->string (car name&attrs))
               (render-attr-name (cdr name&attrs) "")))
	(else (symbol->string name&attrs))))

(define (render-name name&attrs)
  (cond ((pair? name&attrs)
         (symbol->string (car name&attrs)))
        (else (symbol->string name&attrs))))

(define (render-elt elt)
  (cond ((string? elt) (entitize elt))
        ((symbol? elt) (list (entitize (symbol->string elt)) " "))
        ((number? elt) (number->string elt))
	((list? elt)
	 (let ((name&attrs (car elt))
	       (sub-elts (cdr elt)))
	   (if (null? sub-elts)
	       (list "<" (render-name&attrs name&attrs) " />")
	       (list "<" (render-name&attrs name&attrs) ">"
                     (map render-elt sub-elts)
                     "</"
                     (render-name name&attrs)
                     ">"))))
        ((literal? elt) (literal-string elt))
	(else (let ((p (make-string-output-port)))
		(write elt p)
		(string-output-port-output p)))))

(define (xml . elts)
  (tree-string->string
   (map (lambda (elt) (render-elt elt)) elts)))

(define (xml-join list separator)
  (fold-right
   (lambda (item rest)
      (cons item
	    (if (null? rest)
		'()
		(cons separator rest))))
   '()
   list))

(define-record-type literal :literal
  (make-literal s)
  literal?
  (s literal-string set-literal-string!))

(define (literally . s)
  (make-literal s))

(define-syntax template
  (syntax-rules ()
    ((template (A1 A2 ...) PRELUDE ELT1 ELT2 ...)
     (lambda (A1 A2 ...) (string-append PRELUDE (xml ELT1 ELT2 ...))))))
