(library (knight utils)
  (export
    cache-on!
    cache-off!
    get-func-from-cache
    set-func-to-cache!
    encode-html
    call-with-irregex-replace
    file-loader
    string-trim-start
    string-trim-end
    string-trim
    string-split
    string-simplify
    )
  (import
    (scheme)
    (irregex irregex))

  (define cache-status? #t)

  (define cache-table (make-eq-hashtable))
    
  (define cache-on! (lambda () (set! cache-status? #t)))

  (define cache-off! (lambda () (set! cache-status? #f)))

  (define get-func-from-cache
    (lambda (file-name file-time)
      (let* ([key (string->symbol file-name)]
             [time/func (hashtable-ref cache-table key #f)])
        (if (and cache-status? time/func (time=? file-time (car time/func))) 
          (cdr time/func)
          (and (hashtable-delete! cache-table key) #f)))))

  (define set-func-to-cache!
    (lambda (file-name file-time func)
      (let ([key (string->symbol file-name)]
            [value (cons file-time func)])
        (if cache-status?
          (hashtable-set! cache-table key value)
          (hashtable-delete! cache-table key)))))
 
  (define call-with-irregex-replace
    (lambda (reg str func)
      (let ([len (string-length str)])
        (let loop ([match (irregex-search reg str 0 len)]
                   [rst '()]
                   [last 0])
          (if match
            (loop
              (irregex-search reg str (irregex-match-end-index match) len)
              (cons* 
                (func (irregex-match-substring match))              
                (substring str last (irregex-match-start-index match))
                rst)
              (irregex-match-end-index match))
            (apply string-append (reverse (cons (substring str last len) rst)))
          )
        )
      )
    )
  )
  
  (define encode-html-rules
    '((#\& . "&amp;")
      (#\< . "&lt;")
      (#\> . "&gt;")
      (#\" . "&#34;")
      (#\' . "&#39;")))

  (define match-html-regex "[&<>'\"]")

  (define encode-html-char
    (lambda (c)
      (let* ([key (string-ref c 0)]
             [rst (assoc key encode-html-rules)])
        (if rst (cdr rst) c))))

  (define encode-html
    (lambda (html)
      (call-with-irregex-replace match-html-regex html encode-html-char)))

  (define file-loader 
    (lambda (file-name) 
      (let ((p (open-input-file file-name)))
        (let loop ((lst '()) (c (read-char p)))
            (if (eof-object? c)
                (begin 
                    (close-input-port p)
                    (list->string (reverse lst)))
                (loop (cons c lst) (read-char p)))))))

  (define string-trim-start
    (lambda (str)
      (let loop ([idx 0]
                 [len (string-length str)])
        (if (= idx len)
          ""
          (if (char=? #\space (string-ref str idx))
            (loop (+ 1 idx) len)
            (substring str idx len)
          )))))
  
  (define string-trim-end
    (lambda (str)
      (let loop ([idx (- (string-length str) 1)])
        (if (= idx -1)
          ""
          (if (char=? #\space (string-ref str idx))
            (loop (- idx 1))
            (substring str 0 (+ 1 idx))
          )))))

  (define string-trim
    (lambda (str)
      (string-trim-end (string-trim-start str))))
  
  (define string-split
    (lambda (s c)
      (letrec* ([len (string-length s)]
                [walk (lambda (str begin end rst)
                        (cond ((>= begin len) rst)
                              ((or (= end len) (char=? (string-ref str end) c))
                                (walk
                                  str 
                                  (+ end 1)
                                  (+ end 1)
                                  (if (= begin end) 
                                    rst
                                    (cons (substring str begin end) rst))))
                              (else
                                (walk str begin (+ end 1) rst))))])
        (reverse (walk s 0 0 '())))))
  
  (define string-simplify
    (lambda (str)
      (if (and 
            (char=? #\" (string-ref str 0))
            (char=? #\" (string-ref str (- (string-length str) 1))))
        (substring str 1 (- (string-length str) 1))
        str)))
)