;;; Filters (Unix pipes)

; requires safe-fork srfi-8 posix-i/o

(define (safe-fork parent-proc child-thunk)
  (let ((interrupts (set-enabled-interrupts! no-interrupts)))
    (cond ((fork) =>
           (lambda (child)
             (set-enabled-interrupts! interrupts)
             (parent-proc child)))
          (else
           (child-thunk)))))

(define (filter-chain codec text . filters)
  (cond ((null? filters)
         text)
        (else
         (apply filter-chain
		codec
                (apply filter-through
		       codec
		       text
		       (if (list? (car filters))
			   (car filters)
			   (list (car filters))))
                (cdr filters)))))

(define (filter-through codec text program-name . arguments)
  (receive (source-input-port source-output-port)
      (open-pipe)
    (receive (result-input-port result-output-port)
        (open-pipe)
      (let ((interrupts (set-enabled-interrupts! no-interrupts)))
        (cond ((fork) =>
               (lambda (child)
                 (set-enabled-interrupts! interrupts)
                 (close-input-port source-input-port)
                 (close-output-port result-output-port)
                 (set-port-text-codec! source-output-port
                                       codec)
                 (set-port-text-codec! result-input-port
                                       codec)
                 (write-string text source-output-port)
                 (close-output-port source-output-port)
                 (let ((result (read-fully result-input-port)))
                   (close-input-port result-input-port)
                   (wait-for-child-process child)
                   result)))
              (else
               (close-output-port source-output-port)
               (close-input-port result-input-port)
               (remap-file-descriptors! source-input-port result-output-port)
               (apply exec program-name arguments)))))))
