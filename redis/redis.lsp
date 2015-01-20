;; @module redis.lsp
;; @author Ivan Ribeiro Rocha ivan.ribeiro@gmail.com

(context 'redis)

(define (cli p)
    (if (nil? p) "redis-cli" "redis-cli --pipe"))

(define (proto w s f)
    (format (join (list s f "%s" f)) (length w) w))

(define (cmd s stdin)
    (letn ((tokens (parse s))
           (fmt (cons "$%d" "\r\n")))
        (format "*%d%s%s" 
                (length tokens) 
                (last fmt)
                (join (map (lambda (x) (proto x (first fmt) (last fmt))) tokens)))))

(define (cmds l)
    (join (map (lambda (x) (cmd x)) l)))

(define (redis:pipeline l stdin, res)
    (let ((tmp (format "/tmp/redis-%s" (uuid))))
        (delete-file tmp)
        (if (exec (format "%s 2>&1 > %s" (cli true) tmp) (cmds l))
            (begin 
                (do-while (not file? tmp) (sleep 10))
                (setq res (parse (read-file tmp)))
                (delete-file tmp)
                (list (int (nth 14 res)) (int (nth 17 res)))
            ) nil)))

(define (redis:call s)
    (exec (format "%s %s" (cli) s)))

; (redis:call "set foo bar")
; (redis:pipeline '("ping" "set foo bar" "incr ale"))

(context 'MAIN)
