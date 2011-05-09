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
	  tok make-seeker snap trim-back trim-front trim flatten cadr-or-else)
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

(define-structure xml
  (export xml xml-join literally)
  (open scheme tree-strings extended-ports text-codecs util srfi-1
	define-record-types i/o)
  (files "xml.scm"))

(define-structure deferred-tasks
  (export call-later minder)
  (open scheme queues threads time)
  (files "deferred-tasks.scm"))

(define-structure cookies
  (export make-cookie-string make-cookie-string-unescaped split-cookie-pairs)
  (open scheme posix-time sequence-utils tree-strings url-utils)
  (files "cookies.scm"))

(define-structure magic
  (export cleanup define-handler add-response-content-type!
	  add-response-cookie! send current-request send-newline xml
	  literally form submit file field redirect! ajax-link
	  ajax-stateless-link ajax-form minder serve)
  (open scheme srfi-13 srfi-14 fluids sockets i/o primitives threads
	byte-vectors encodings text-codecs extended-ports queues srfi-1
	posix-time ascii define-record-types cells handle srfi-27 srfi-8
	posix-files posix-regexps tables

	time simple-signals os-strings io-utils property-lists sequence-utils
	tree-strings process-utils url-utils xml deferred-tasks cookies)
  (files "magic.scm"))

(define-structure magic-demos
  (export start)
  (open scheme srfi-13 srfi-14 fluids sockets i/o primitives threads
	byte-vectors encodings text-codecs extended-ports queues srfi-1
	posix-time ascii define-record-types cells handle srfi-27 srfi-8
	posix-files posix-regexps tables

	magic
	time simple-signals os-strings io-utils property-lists sequence-utils
	tree-strings process-utils url-utils xml deferred-tasks cookies)
  (files "magic-demos.scm"))
