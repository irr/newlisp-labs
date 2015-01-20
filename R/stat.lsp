#!/usr/bin/env newlisp

(define Plan:Plan)

(define (sanitize t)
    (let ((k (first t)))
        (pop t)
        (do-while (and (> (length t) 0) (= (first t) "") (pop t)))
        (Plan k (filter (fn (x) (not (nil? x))) (map 'float t)))))

(define (parse-plan f, t)
    (let ((lines (parse (read-file f) "\n")))
        (dolist (l lines)
            (setq t (parse l "\t"))
            (if (> (length t) 0)
                (Plan (first t) (sanitize t))))))

(if (< (length (main-args)) 3)
    (begin (print "usage: stats.lsp <file>\n") (exit 1))
    (let ((f (string (main-args 2)))) 
        (parse-plan f)))
