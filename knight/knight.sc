(library (knight knight)
  (export
    cache-on!
    cache-off!
    set-template-path!
    set-template-suffix!
    render
    render-file
    )
  (import
    (scheme)
    (irregex irregex)
    (knight utils))

  (define default-template-path ".")
  
  (define default-template-suffix "ejs")

  (define default-delimiter #\%)

  (define regex-string "(<%%|<%=|<%-|<%#|<%)\\s*?(.)+?\\s*?%>")

  (define set-template-path! (lambda (path) (set! default-template-path path)))

  (define set-template-suffix! (lambda (suffix) (set! default-template-suffix suffix)))

  ;; 获取确定存在文件地址
  (define get-include-path
    (case-lambda
      ((path parent-path)
        (get-include-path path parent-path #t)) 
      ((path parent-path debug?)
        (if (file-exists? path)
          path
          (let* ([ext (path-extension path)]
                 [file-name (if (and (string=? ext "") (not (string=? default-template-suffix "")))
                              (format "~a.~a" path default-template-suffix)
                              path)]
                 [file-path (if (file-exists? file-name) 
                              file-name 
                              (format "~a~a~a" parent-path (directory-separator) file-name))])
            (if (file-exists? file-path)
              file-path
              (if debug? (error 'get-include-path (format "Could not find the file: ~a" file-path)) #f)
            ))
        ))))

  ;; 编译字符串模板
  (define render
    (case-lambda 
      ((html content-map)
        (render html content-map html (make-time 'time-duration 0 0)))
      ((html content-map file-path file-time)
        (let ([func (get-func-from-cache file-path file-time)])
          (if func
            (func content-map)
            ((template-compile html `((file-path . ,file-path) (html . ,html) (file-time . ,file-time)))
              content-map)
            )))))

  ;; 编译模板文件
  (define render-file
    (lambda (file-name content-map)
      (let* ([file-path (get-include-path file-name default-template-path)])
        (render (file-loader file-path) content-map file-path (file-change-time file-path)))))

  ;; include 语法
  (define render-include-file
    (lambda (file-name content-map options)
      (let* ([current-path (get-include-path file-name (path-parent (options-ref options "file-path")) #f)]
             [path (or current-path (get-include-path file-name default-template-path))])
        (render-file path content-map)      
      )
    ))

  ;; source->function
  (define template-compile
    (lambda (html options)
      (let* ([parse-list (parse-template html)]
             [func (generate-function parse-list options)])
        (set-func-to-cache! (options-ref options "file-path") (options-ref options "file-time") func)
        func
      )
    ))

  ;; 生成函数字符串
  (define generate-source (lambda () (void)))
  
  ;; 生成函数
  (define generate-function 
    (lambda (parse-list options)
      (let ([lst (map parse-line parse-list)])
        (lambda (content-map)
          (apply
            string-append
            (map 
              (lambda (line)
                (cond
                  ((string? line) line)
                  ((pair? line)
                    (case (car line)
                      (#\# "") ; 注释
                      (#\= (encode-html (eval-line line content-map options)))  ; 转义输出
                      (#\- (eval-line line content-map options)) ; 不转义输出
                      (#\% (encode-html "<%"))                            ; 输出<%
                      (else (eval-line line content-map options)) ; 默认不转义
                      ))
                  ))
              lst
            )
          )))))

  ;; 解析模板文件
  (define parse-template
    (lambda (html)
      (let ([len (string-length html)]
            [reg regex-string])
        (let loop ([match (irregex-search reg html 0 len)]
                   [rst '()]
                   [last 0])
          (if match
            (loop
              (irregex-search reg html (irregex-match-end-index match) len)
              (cons* 
                match              
                (substring html last (irregex-match-start-index match))
                rst)
              (irregex-match-end-index match))
            (reverse (cons (substring html last len) rst))
          )
        )
      )
    )
  )
 
  ;; 解析当前行
  (define parse-line
    (lambda (line)
      (if (string? line)
        line
        (let* ([match (irregex-match-substring  line)]
               [tag1 (substring match 0 (- (vector-ref line 10) (vector-ref line 8)))]
               [tag2 (substring match 
                        (- (vector-ref line 10) (vector-ref line 4))
                        (- (vector-ref line 6) 2 (vector-ref line 4)))]
               [type (if (> (string-length tag1) 2) (string-ref tag1 2) #\s)]
               [content (string-trim tag2)]
               [include? (and (> (string-length content) 8) (string-ci=? "include " (substring content 0 8)))]
               [include-path (if include? (string-simplify (string-trim (substring content 8 (string-length content)))) "")])
          (if include?
            (list type 'include include-path)
            (list type 'eval content)
          )
        )
      )
    ))

     ;; 分析当行模板文件
  (define eval-line
    (lambda (line content-map options)
      (case (cadr line)
        (eval (params-ref content-map (caddr line)))
        (include (render-include-file (caddr line) content-map options))
        (else (error 'eval-line "unknown line command"))
      )))
  
  (define options-ref
    (case-lambda
      ([alst key] (options-ref alst key ""))
      ([alst key default] 
        (let* ([string-key (if (string? key) key (symbol->string key))]
               [symbol-key (string->symbol string-key)]
               [rst (or (assoc symbol-key alst) (assoc string-key alst))])
          (if rst (cdr rst) default)))))
          
  (define params-ref options-ref)
)