(define *to-do-list* (make-queue))

(define (call-later millis thunk)
  (let ((pair (cons (+ (real-time)
                       millis)
                    thunk)))
    (enqueue! *to-do-list* pair)))

(define *should-mind-list* #t)
(define *minder-nap-duration* 1000)

(define (minder)
  (let loop ()
    (cond ((not *should-mind-list*)
           'done)
          ((not (queue-empty? *to-do-list*))
           (let ((pair (queue-head *to-do-list*)))
             (cond ((> (real-time)
                       (car pair))
                    (dequeue! *to-do-list*)
                    ((cdr pair)))
                   (else
                    (sleep *minder-nap-duration*))))
           (loop))
          (else
           (sleep *minder-nap-duration*)
           (loop)))))
