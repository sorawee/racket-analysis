#lang racket/base

(require racket/cmdline
         racket/function
         racket/match
         racket/date
         racket/list
         racket/pretty
         racket/format
         racket/string
         syntax/parse
         syntax/parse/define
         drracket/check-syntax)

;; get-syntax :: path-string? -> (or/c #f syntax?)
(define (get-syntax path)
  (parameterize ([read-accept-reader #t])
    (with-handlers ([exn:fail? (const #f)])
      (call-with-input-file* path
        (λ (p)
          (port-count-lines! p)
          (define result (read-syntax (object-name p) p))
          (cond
            [(eof-object? result) #f]
            [else result]))))))

;; info/c = (list (listof identifier?) check-syntax-output?)

;; get-info :: syntax? -> (or/c #f info/c)
(define (get-info stx)
  (define ns (make-base-namespace))
  (define expanded-stx
    (with-handlers ([exn:fail? (const #f)])
      (parameterize ([current-namespace ns])
        (expand stx))))
  (cond
    [expanded-stx
     (define source (syntax-source stx))
     (define mapping (make-hash))
     (let loop ([stx expanded-stx])
       (define (check-property prop-key)
         (define props (and (syntax? stx) (syntax-property stx prop-key)))
         (when props (loop props)))
       (check-property 'disappeared-use)
       (check-property 'disappeared-binding)
       (check-property 'origin)
       (syntax-parse stx
         [(a . b)
          (loop #'a)
          (loop #'b)]
         [() (void)]
         [x:id
          #:when (equal? source (syntax-source #'x))
          (hash-update! mapping
                        (syntax-position #'x)
                        (λ (old) (cons #'x old))
                        '())]
         [_ (void)]))
     (list (append*
            (for/list ([(k v) (in-hash mapping)])
              (remove-duplicates v free-identifier=?)))
           ;; we can draw the arrow ourselves by walking the
           ;; fully expanded syntax but why reinventing the wheel?
           ;; let's just reuse whatever check-syntax gives us
           ;;
           ;; NOTE: currently we don't need this result yet, so make it #f
           ;; to speedup analysis
           #f
           #;(show-content expanded-stx
                           #:fully-expanded? #t
                           #:namespace ns))]
    [else #f]))

(struct issue (level message stx) #:transparent)

;; member-by-position :: identifier? (listof identifier?) -> boolean?
(define (member-by-position x ids)
  (for/or ([id (in-list ids)])
    (equal? (syntax-position x)
            (syntax-position id))))

(struct analyzer (lv name proc))

(define-simple-macro (create-analyzer #:level lv #:name name body ...+)
  (analyzer 'lv 'name (let () body ...)))


;; analyze :: info/c -> (listof issue?)
(define (analyze info stx)
  (match-define (list ids _check-syntax-out) info)

  (define analyzers
    (list
     (create-analyzer
      #:level warning #:name match:bind-empty-list
      (define match-ids (filter (λ (id) (free-identifier=? id #'match)) ids))
      (syntax-parser
        [(-match _ _ ... [{~or {~datum null} {~datum empty}} _ ...+] _ ...)
         #:when (member-by-position #'-match match-ids)
         (list this-syntax)]
        [_ '()]))

     (create-analyzer
      #:level warning #:name case:quote
      (define case-ids (filter (λ (id) (free-identifier=? id #'case)) ids))
      (syntax-parser
        [(-case _ _ ... [({~datum quote} _) _ ...+] _ ...)
         #:when (member-by-position #'-case case-ids)
         (list this-syntax)]
        [_ '()]))

     (create-analyzer
      #:level error #:name cond:invalid-else
      (define cond-ids (filter (λ (id) (free-identifier=? id #'cond)) ids))
      (define else-ids (filter (λ (id) (free-identifier=? id #'else)) ids))
      (syntax-parser
        [(-cond _ ... [{~and -else {~datum else}} _ ...+])
         #:when (and (member-by-position #'-cond cond-ids)
                     (not (member-by-position #'-else else-ids)))
         (list this-syntax)]
        [_ '()]))

     (create-analyzer
      #:level error #:name case:invalid-else
      (define case-ids (filter (λ (id) (free-identifier=? id #'case)) ids))
      (define else-ids (filter (λ (id) (free-identifier=? id #'else)) ids))
      (syntax-parser
        [(-case _ ... [{~and -else {~datum else}} _ ...+])
         #:when (and (member-by-position #'-case case-ids)
                     (not (member-by-position #'-else else-ids)))
         (list this-syntax)]
        [_ '()]))

     (create-analyzer
      #:level annoying #:name cond:no-else
      (define cond-ids (filter (λ (id) (free-identifier=? id #'cond)) ids))
      (define else-ids (filter (λ (id) (free-identifier=? id #'else)) ids))
      (syntax-parser
        [(-cond _ ... [-something _ ...])
         #:when (and (member-by-position #'-cond cond-ids)
                     (not (member-by-position #'-something else-ids)))
         (list this-syntax)]
        [_ '()]))

     (create-analyzer
      #:level error #:name match-like:bind-else
      (define match-like-ids
        (filter (λ (id) (or (free-identifier=? id #'match)
                            (free-identifier=? id #'syntax-case)
                            (free-identifier=? id #'syntax-parse)
                            (free-identifier=? id #'syntax-parser)
                            (free-identifier=? id #'define-syntax-parser))) ids))
      (syntax-parser
        [(-match-like _ ... [{~datum else} _ ...+])
         #:when (member-by-position #'-match-like match-like-ids)
         (list this-syntax)]
        [_ '()]))))

  (let loop ([stx stx])
    (append*
     (syntax-parse stx
       [(a . b) (append (loop #'a) (loop #'b))]
       [_ '()])
     (for/list ([the-analyzer (in-list analyzers)])
       (match the-analyzer
         [(analyzer lv name proc)
          (cond
            [(and (or (null? (current-levels))
                      (member lv (current-levels)))
                  (or (null? (current-analyzers))
                      (member name (current-analyzers))))
             (for/list ([stx-fragment (in-list (proc stx))])
               (issue lv name stx-fragment))]
            [else '()])]
         [_
          ;; currently not supported
          '()])))))

(define (main path)
  (for ([f (in-directory path)]
        #:when (string-suffix? (~a f) ".rkt")
        ;; This directory alone takes like 10+ mins. Let's skip it.
        #:unless (string-contains? (~a f) "pkgs/racket-test/tests/racket/stress/")
        ;; Not interested in trash
        #:unless (string-contains? (~a f) "/.trash/"))
    (define stx (get-syntax f))
    (when stx
      (define info (get-info stx))
      (when info
        (when (current-log-file)
          (with-output-to-file (current-log-file) #:exists 'append
            (λ () (printf "[~a] ~a\n" (date->string (current-date) #t) f))))
        (define results (analyze info stx))
        (unless (null? results)
          (displayln "----------------------")
          (printf "[~a] Processing ~a\n" (date->string (current-date) #t) f)
          (for-each pretty-print results)
          (newline))))))

(define current-analyzers (make-parameter '()))
(define current-levels (make-parameter '()))
(define current-log-file (make-parameter #f))

(module+ main
  (define path
    (command-line
     #:multi
     [("--analyze") name
                    "Analyzer name (default: every analyzer)"
                    (current-analyzers (cons (string->symbol name) (current-analyzers)))]
     [("--level") level
                  "Level (default: every level)"
                  (current-levels (cons (string->symbol level) (current-levels)))]
     #:once-each
     [("--log-file") the-log-file
                     "Logfile (default: none)"
                     (current-log-file the-log-file)]
     #:args (path)
     path))

  ;; make sure the file is created
  (when (current-log-file)
    (with-output-to-file (current-log-file) #:exists 'append
      (λ () (display ""))))

  (main path))
