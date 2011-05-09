
;;; Sample code

;; General purpose request dumper that sets a cookie with k & v if
;; present.

(define-handler (dump k v)
  (add-response-content-type! "text/plain")
  (if (and k v) (add-response-cookie! (string->symbol k) v))
  (send (current-request) write)
  (send-newline))

;; A simple XHTML page template.

(define jquery-url "http://code.jquery.com/jquery-1.6.min.js")

(define (page title . body)
  (send "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
        \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
")
  (send xml `((html xmlns "http://www.w3.org/1999/xhtml"
                    xml:lang en
                    lang en)
              (head ((meta http-equiv "Content-Type"
                           content "text/html;charset=utf-8"))
                    (title ,title)
                    ,(js-include jquery-url)
                    ,(js-include "http://poseur.com/js/magic.js"))
              (body (div
                     ((img src "http://poseur.com/img/magic.png"
                           style "float: right" alt "magic logo"))
                     (h1 ,title))
                    ,@body))))

;; Acid test

(define-handler (acid-test)
  (send xml
   `((form action dump
           method post
           enctype multipart/form-data)
     f: ,(file f) (br)
     x: ,(field x 42) (br)
     y: ,(field y 1) (br)
     g: ,(file g) (br)
     z: ,(field z 43) (br)
     h: ,(file h) (br)
     ,(submit "Go"))))

;; Redirect test

(define-handler (redirect)
  (redirect! "http://google.com/"))

;; Front page

(define-handler (index)
  (page
   "Magic 3: A web framework for Scheme 48"
   '(p "Magic 3 is a framework for writing web applications."
       " It meets the "
       ((a href "http://www.paulgraham.com/arcchallenge.html")
        "Arc Challenge")
       " quite nicely, thank you.")
   '(pre
     (code
      "(define-handler (" ((a href said) "said") ")" (br)
      "  (page" (br)
      "   \"Arc Challenge\"" (br)
      "   (ajax-form 'output (message)" (br)
      "              `(,(field message) ,(submit \"Go\"))" (br)
      "              `(p" (br)
      "                ,(ajax-link \"Click here\" 'output" (br)
      "                            `(p \"You said: \" ,message))))" (br)
      "   '((div id output))))" (br)
      ))
  '(p "The " ((a href "http://github.com/edw/magic/")
	      "source code")
      " is WTFPL-licensed and is free of external dependencies on any other"
      " packages, so you can get up and running quickly. Have fun.")))

;; Train schedule

(define (ul . items)
  `(ul ,@(map (lambda (item) `(li ,@(if (list? item)
                                        item
                                        (list item))))
              items)))

;; (define (js . lines)
;;   `((script type "text/javascript")
;;     ,@(fold (lambda (a b) ()))))

(define (js-include url)
  `((script type "text/javascript" src ,url) ""))

(define (css-include url)
  `((link rel "stylesheet" type "text/css" href ,url)))

(define *tz-adjustment* (*  3 60 60))

(define (local-time)
  (let ((s (time->string (make-time (+ *tz-adjustment* (time-seconds (current-time)))))))
    (map string->number (list
      (substring s 11 13)
      (substring s 14 16)))))

(define (format-local-time lt)
  (string-append (number->string (car lt))
                 ":"
                 (if (> (cadr lt) 9)
                     ""
                     "0")
                 (number->string (cadr lt))))

(define (format-minutes m)
  (let ((h (quotient m 60))
        (m (remainder m 60)))
    (string-append
     (number->string h)
     ":"
     (if (> m 9) "" "0")
     (number->string m))))

(define (local-time-difference smaller bigger)
  (+ (* 60 (- (car bigger) (car smaller)))
     (- (cadr bigger) (cadr smaller))))

(define sched
  (list '("Market East to Norristown" .
          ((7 07) (7 40) (8 08) (8 56) (9 45) (10 30) (11 30)))
        '("Conshohocken to Philadelphia" .
          ((15 43) (16 48) (17 15) (17 53) (18 21) (19 15) (20 00)))))

(define (sched->xml)
  (let ((lt (local-time)))
    (define (process-sched-time t)
      (let ((diff (local-time-difference lt t)))
        (cond ((> diff 0)
               (list
                (format-local-time t)
                " ("
                (format-minutes diff)
                ")"))
              (else '()))))
    (cons
     'dl
     (flatten (map
               (lambda (e) `((dt ,(car e)) (dd (ul ,@(cdr e)))))
               (filter (lambda (e) (> (length e) 1))
                       (map (lambda (e)
                              (cons (car e)
                                    (map
                                     (lambda (e) `(li ,@e))
                                     (filter
                                      (lambda (e) (not (null? e)))
                                      (map process-sched-time (cdr e))))))
                            sched)))))))

(define-handler (r6)
  (send (xml `(html (head (title "R6 Schedule"))
                (body ,(sched->xml))))))

;; Wiki

(define (raw-wiki-page page-name)
  (add-response-content-type! "text/plain")
  (send (read-file page-name)))

(define (wiki-page page-name)
  (let ((body (markdown-file page-name)))
    (send "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
        \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
")
    (send xml `((html xmlns "http://www.w3.org/1999/xhtml"
                      xml:lang en
                      lang en)
                (head ((meta http-equiv "Content-Type"
                             content "text/html;charset=utf-8"))
                      (title "Wiki: ",page-name)
                      ,(css-include "/wraw/wiki-css")
                      ,(js-include jquery-url)
                      ,(js-include "http://poseur.com/js/magic.js"))
                (body ((div id "wikictls")
                       (p "This is a wiki page. "
                          ,(ajax-stateless-link
                            'editdiv
                            (string->symbol (string-append "weditinplace/"
                                                           page-name))
                            "Edit It." 'focus (string->symbol "#editdiv"))
                          " Or consider going to the "
                          ((a href "/wiki/") "front page")
                          " or a list of "
                          ((a href "/wpages") "every page")
                          "."))
                      ,@body
                      ((div id "editdiv")))))))

(define *evil-path-name* "front")
(define *wiki-dir* "var/wiki/")

(define *wiki-link-re* (make-regexp "\\{\\{([a-zA-Z-]+)\\}\\}" 
                                   (regexp-option submatches)
                                   (regexp-option extended)))

(define (replace-wiki-links content)
  (define (make-wiki-link match)
    (let ((page-name
           (substring content (match-start match) (match-end match))))
      (string-append "[" page-name "](/wiki/" page-name ")")))
  (let loop ((i 0) (s ""))
    (let ((matches (regexp-match *wiki-link-re* content i #t #f #f)))
      (if matches
          (loop (match-end (car matches))
                (string-append
                 s
                 (substring content i (match-start (car matches)))
                 (make-wiki-link (cadr matches))))
          (string-append s (substring content i (string-length content)))))))

(define (tainted-path? path)
  (or (not path) (not (not (string-contains path "../")))))

(define (write-wiki-file path-name content)
  (if (not (tainted-path? path-name))
      (call-with-output-file (string-append *wiki-dir* path-name)
        (lambda (port)
          (write-string content port)))))

(define (read-file path-name)
  (let ((path-name (string-append *wiki-dir* path-name)))
    (if (accessible? path-name (access-mode exists))
        (call-with-input-file
            path-name
            (lambda (port)
              (set-port-text-codec! port utf-8-codec)
              (read-fully port)))
        #f)))

(define (markdown t)
  (filter-chain utf-8-codec
		t "var/wiki/bin/Markdown.pl" "var/wiki/bin/SmartyPants.pl"))

(define (markdown-file page-name)
  (let ((file-contents (markdown (replace-wiki-links (or (read-file page-name)
                                      "# Not Found

Sorry, we couldn't find anything by that name.")))))
    `(,(literally file-contents))))

(define-handler (wiki p)
  (let ((p (or p
                (symbol->string (cadr-or-else 
                                 (get-prop (current-request) 'path)
                                 'front)))))
    (if (tainted-path? p)
        (redirect! (string-append "/wiki/" *evil-path-name*))
        (wiki-page p))))

(define *wiki-pw* "password")

(define-handler (wraw p)
  (let ((p (or p
               (symbol->string (cadr-or-else
                                (get-prop (current-request) 'path)
                                'front)))))
    (if (tainted-path? p)
        (redirect! (string-append "/wraw/" *evil-path-name*))
        (raw-wiki-page p))))

(define-handler (wedit p pw)
  (define (get-path)
    (cond (p
           (if (tainted-path? p)
               'front
               p))
          ((cadr-or-else (get-prop (current-request) 'path) #f)
           => symbol->string)
          (else 'front)))
  (let* ((p (get-path))
         (page-content (read-file p)))
    (page "Edit Page"
          `(h2 "Page name: "
               ,p
               ,@(if (not page-content)
                     '(" (this will be a new page)")
                     '()))
          (form (page-content pw)
                `(((textarea rows 20 cols 80 name "page-content")
                   ,(if (not page-content) "" page-content))
                  (p ((input type submit value "Save"))
                     ((input type password name pw value ,(or pw "")))))
                (cond ((and (string? pw) (string=? pw *wiki-pw*))
                       (write-wiki-file p page-content)
                       (add-response-cookie! 'pw pw)
                       (redirect! (string-append "/wiki/" p)))
                      (else
                       (page "Wrong Password" '(p "Try again."))))))))
(define-handler (weditinplace p pw)
  (define (get-path)
    (cond (p
           (if (tainted-path? p)
               'front
               p))
          ((cadr-or-else (get-prop (current-request) 'path) #f)
           => symbol->string)
          (else 'front)))
  (let* ((p (get-path))
         (page-content (read-file p)))
    (send xml
          '(hr)
          (if (not page-content)
                '(h2 " (this will be a new page)")
                "")
          ;; Write a STATELESS-AJAX-FORM with key-value pairs to
          ;; submit so we can avoid having to time out. (Hmm, this Arc
          ;; Challenge stuff causes garbage to collect on the server
          ;; and we do everything we can to avoid using it... Hmm...)
          (form (page-content pw)
                `(((textarea rows 20 cols 80 name "page-content")
                   ,(if (not page-content) "" page-content))
                  (p ((input type submit value "Save"))
                     ((input type password name pw value ,(or pw "")))))
                (cond ((and (string? pw) (string=? pw *wiki-pw*))
                       (write-wiki-file p page-content)
                       (add-response-cookie! 'pw pw)
                       (redirect! (string-append "/wiki/" p)))
                      (else
                       (page "Wrong Password" '(p "Try again."))))))))

(define (list-regular-files dirname)
  (define (regular-file? filename)
    (let ((finfo (get-file-info (string-append *wiki-dir* filename))))
      (eq? (file-info-type finfo) (file-type regular))))
  (let loop ((dir (open-directory-stream dirname))
             (files '()))
    (let ((el (read-directory-stream dir)))
      (cond ((not el)
             (close-directory-stream dir)
             (reverse files))
            (el
             => (lambda (el)
                  (let ((filename (os-string->string el)))
                    (loop dir
                          (if (regular-file? filename)
                              (cons filename files)
                              files)))))))))

(define-handler (wpages)
  (page "Wiki Pages"
        `(ul ,@(map (lambda (el)
                      `(li ((a href ,(string-append "/wiki/" el)) ,el)))
                    (list-regular-files *wiki-dir*)))))

(define (start pw)
  (spawn minder)
  (spawn serve)
  (set! *wiki-pw* pw))

(define-handler (said)
  (page
   "Arc Challenge"
   (ajax-form 'output (message)
              `(,(field message) ,(submit "Go"))
              `(p
                ,(ajax-link "Click here" 'output
                            `(p "You said: " ,message))))
   '((div id output))))

;; End of file
