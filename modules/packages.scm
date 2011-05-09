(define-structure css
  (export write-css css->string)
  (open scheme srfi-6)
  (files "css.scm"))

(define-structure io-utils
  (export read-fully read-line)
  (open scheme silly)
  (files "io-utils.scm"))

(define-structure property-lists
  (export add-prop get-prop remove-prop for-matching-props)
  (open scheme)
  (files "property-lists.scm"))

(define-structure sequence-utils
  (export starts-with? string-downcase byte-vector->string char-in-list?
	  tok make-seeker snap trim-back trim-front trim flatten)
  (open scheme silly queues srfi-1 encodings)
  (files "sequence-utils.scm"))

(define-structure tree-strings
  (export tree-string-length tree-string->string tree-string-walk)
  (open scheme)
  (files "tree-strings.scm"))

(define-structure process-utils
  (export safe-fork filter-chain filter-through)
  (open scheme srfi-8 posix interrupts i/o io-utils)
  (files "process-utils.scm"))

(define-structure url-utils
  (export url-encode url-decode)
  (open scheme silly util i/o extended-ports)
  (files "url-utils.scm"))

(define-structure magic
  (export cleanup define-handler add-response-content-type!
	  add-response-cookie! send current-request send-newline page xml
	  literally form file field redirect! ajax-link ajax-form minder serve)
  (open scheme srfi-13 srfi-14 fluids sockets i/o primitives threads
	byte-vectors encodings text-codecs extended-ports queues srfi-1
	posix-time ascii define-record-types cells handle srfi-27 srfi-8
	posix-i/o interrupts posix-processes posix-files posix-regexps tables
	time queues simple-signals os-strings io-utils property-lists
	sequence-utils tree-strings process-utils url-utils)
  (files "magic.scm"))

