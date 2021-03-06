(define-module (spec suite)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:export (describe
            it
            expect
            spec-run))

(define-record-type <spec-suite>
  (make-suite description before-each after-each specs)
  suite?
  (description suite-description)
  (before-each suite-before-each)
  (after-each suite-after-each)
  (specs suite-specs))

(define-record-type <spec-spec>
  (make-spec description test)
  spec?
  (description spec-description)
  (test spec-test))

(define-record-type <spec-results>
  (make-results pass fail errors)
  spec-results?
  (pass results-pass)
  (fail results-fail)
  (errors results-errors))

(define-record-type <spec-error>
  (make-spec-error description key args stack)
  spec-error?
  (description spec-error-description)
  (key spec-error-key)
  (args spec-error-args)
  (stack spec-error-stack))

(define (empty-results)
  (make-results 0 0 '()))

(define (combine-results a b)
  (make-results (+ (results-pass a)
                   (results-pass b))
                (+ (results-fail a)
                   (results-fail b))
                (append (results-errors a)
                        (results-errors b))))

(define (expect predicate . args)
  (unless (apply predicate args)
    (throw 'expect-error predicate args)))

(define (indent-display text indent)
  (display (make-string indent #\tab))
  (display text)
  (newline))

(define (display-spec-error error)
  (display (make-string 20 #\=))
  (newline)
  (display "Spec: ")
  (display (spec-error-description error))
  (newline)
  (display "Exception: ")
  (display (spec-error-key error))
  (newline)
  (pretty-print (spec-error-args error))
  (newline)
  (display-backtrace (spec-error-stack error) (current-output-port))
  (newline)
  (newline))

(define (prefix-description prefix description)
  (define (separator)
    ;; Don't add a space if prefix is empty.
    (if (string=? prefix "")
        ""
        " "))
  (string-append prefix (separator) description))

(define (spec-run suites)
  (define (print-results results)
    (newline)
    (format #t "~d passing specs\n" (results-pass results))
    (format #t "~d failing specs\n" (results-fail results))
    (newline)
    (display "Errors:\n")
    (newline)
    (for-each display-spec-error (results-errors results)))
  (print-results (reduce combine-results (empty-results)
                         (map (lambda (suite) (run-suite suite "" 0)) suites))))

(define (run-suite suite desc-prefix indent)
  (define (before-each)
    (when (procedure? (suite-before-each suite))
      ((suite-before-each suite))))
  (define (run-child child)
    (define (full-desc)
      (prefix-description desc-prefix (suite-description suite)))
    (before-each)
    (let ((indent (1+ indent)))
      (cond ((suite? child) (run-suite child (full-desc) indent))
            ((spec? child) (run-spec child (full-desc) indent)))))
  (indent-display (suite-description suite) indent)
  (reduce combine-results (empty-results)
          (map run-child (suite-specs suite))))

(define (run-spec spec desc-prefix indent)
  (define (spec-text prefix)
    (string-append prefix (spec-description spec)))
  (define (spec-pass)
    (indent-display (spec-description spec) indent)
    (make-results 1 0 '()))
  (define (spec-fail key args stack)
    (define (description)
      (prefix-description desc-prefix (spec-description spec)))
    (indent-display (spec-text "FAIL ") indent)
    (make-results 0 1 (list (make-spec-error (description) key args stack))))
  ;; We don't want to let an exception crash our test run.
  ;; Instead we'll capture the stack and save it to be printed out
  ;; after all of the tests have run.
  (let ((stack #f))
    (catch #t
      (lambda ()
        ((spec-test spec))
        (spec-pass))
      (lambda (key . args)
        (pretty-print args)
        (spec-fail key args stack))
      (lambda (key . args)
        (set! stack (make-stack #t))))))

;; Author: Mark Weaver
;; aka the most helpful person on #guile
(define-syntax-parameter it
  (lambda (stx)
    (syntax-violation 'it "it used outside of describe form" stx)))

(define-syntax %describe
  (syntax-rules (before-each after-each)
    ((%describe desc before after (before-each body ...) rest ...)
     (%describe desc (lambda () body ...) after rest ...))
    ((%describe desc before after (after-each body ...) rest ...)
     (%describe desc before (lambda () body ...) rest ...))
    ((%describe desc before after spec ...)
     (make-suite desc
                 before
                 after
                 (syntax-parameterize
                     ((it (syntax-rules ()
                            ((it it-desc it-body (... ...))
                             (make-spec it-desc
                                        (lambda () it-body (... ...)))))))
                   (list spec ...))))))

(define-syntax-rule (describe desc rest ...)
  (%describe desc #f #f rest ...))
