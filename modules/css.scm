;;
;; Sass in Scheme, Take 3
;; Copyright 2011 Edwin Watkeys. All rights reserved.
;; 
;; [Imagine the two-clause MIT license inserted here.]
;;
;; CSS can be a pain. There is apparently a tool called Sass that some
;; people use to deal with some of the issues that accompany non-trivial
;; applications of CSS.
;;
;; For fun, I decided to write a Sass-like tool in Scheme. This is my
;; second take on the problem. I wrote the first version in response to a
;; Hacker News submission:
;;
;; http://news.ycombinator.com/item?id=2514799)
;;
;; My comments on that submission got pummeled with down-votes by people
;; who don't know how to spell "syntactic" and apparently easily take
;; offense and would rather have a community filled with polite morons
;; than people who call bullshit when they see it. Letting people down-
;; vote simply because they've squirreled away enough karma through
;; hundreds of innocuous—and superfluous—comments is no way to run an
;; operation like Hacker News. God knows I'll be tempted to down-vote
;; the shit out of anyone who displeases me if I ever manage to whore
;; my way to five-hundred or whatever karma.
;;
;; So anyway, consider this the extended fuck you re-mix. Enjoy!
;;
;; The original version (https://gist.github.com/956113) required too
;; many parentheses. Compare the old usage:
;; 
;; (make-css
;;  '(("body.loading"
;;     (font-size "12px")
;;     (color "#fff")
;;     (" ul#sidenav"
;;      (" li"
;;       (blah "blah"))))))
;; 
;; To the new:
;; 
;; (string->css
;;  '(("body.loading"
;;     font-size "12px"
;;     color "#fff"
;;     (" ul#sidenav"
;;      (" li"
;;       blah "blah")))))
;; 
;; Both return the same output:
;; 
;; body.loading { font-size: 12px; color: #fff; }
;; body.loading ul#sidenav li { blah: blah; }
;; 
;; Note that you can use all the standard powers of Scheme, such as
;; variables and procedured in concert with quasi-quotation to serve all
;; of your composition needs. E.g.:
;; 
;; (string->css
;;  (let ((base-font-size "12px")
;;        (random-color (lambda () "#fff"))
;;        (blah-declaration '(blah "blah")))
;;    `(("body.loading"
;;       font-size ,base-font-size
;;       color ,(random-color)
;;       (" ul#sidenav"
;;        (" li"
;;         ,@blah-declaration))))))
;;
;; The above, when evaluated, produces identical output to the first
;; example. I believe that this gives you all of the compositional
;; power of Sass.
;;
;; This is a re-write of the second version
;; (https://gist.github.com/957123) that uses ports instead
;; of building up the output using string concatenation, which, as
;; Riastradh pointed out, would lead to very poor performance on
;; highly-nested rule sets. This version offers two procedures:
;;
;; (WRITE-CSS RULES [PORT]) ; returns result of (values)
;; (CSS->STRING RULES) ; returns a string
;;

(define (write-string s p)
  (display s p))

(define (write-strings p . strings)
  (for-each (lambda (s) (write-string s p)) strings))

(define (write-css rules . more)
  (let ((port (if (null? more) (current-output-port) (car more))))
    (define (write-rule rule port ctx)
      (let ((selector (string-append ctx (car rule)))
	    (children-port (open-output-string)))
	(let loop ((decls (cdr rule)) (ndecls 0))
	  (if (null? decls)
	      (begin (if (zero? ndecls) (values) (write-string " }\n" port))
		     (close-output-port children-port)
		     (write-string (get-output-string children-port) port))
	      (let ((head (car decls)))
		(if (pair? head)
		    (begin (write-rule head children-port selector)
			   (loop (cdr decls) ndecls))
		    (begin (if (zero? ndecls)
			       (write-strings port selector " {")
			       (values))
			   (write-strings port " " (symbol->string head)
					  ": " (cadr decls) ";")
			   (loop (cddr decls) (+ ndecls 1)))))))))
    (let loop ((rules rules))
      (if (null? rules) (values)
	   (begin (write-rule (car rules) port "") (loop (cdr rules)))))))

 (define (css->string rules)
   (let ((port (open-output-string)))
     (write-css rules port)
     (close-output-port port)
     (get-output-string port)))
