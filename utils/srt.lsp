#!/usr/bin/env newlisp

(define (charset f)
    (last (parse (first (exec (format "file -bi \"%s\"" f))) "charset=")))

(define (convert f)
    (letn ((content (read-file (format "/tmp/%s.bak" f))))
        (replace "<.*?i>|<.*?b>|<.*?u>" content "" (| 1 2 2048))))

(define (iconv s t f)
    (exec (format "iconv -f %s -t %s//TRANSLIT < \"%s\" > \"/tmp/%s.bak\"" s t f f)))

(define (transform f)
    (let (cs (charset f))
        (begin
            (iconv cs "UTF8" f)
            (if (= (file-info (format "/tmp/%s.bak" f) 0) 0)
                (begin (delete-file (format "/tmp/%s.bak" f)) 
                       (exit 1))
                (let (r (convert f))
                     (begin (exec (format "zenity --info --text='Filename: %s\nEncoding: %s'" f cs))
                            r))))))

(define (srt f)
    (write-file f (transform f))
    (delete-file (format "/tmp/%s.bak" f)))

(if (< (length (main-args)) 3)
    (print "Usage: srt.lsp <file>\n")
    (let ((f (string (main-args 2)))) 
        (srt f)))

(exit 0)
