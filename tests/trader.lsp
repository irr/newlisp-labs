(module "sqlite3.lsp")

(constant '*DB* "/home/irocha/newlisp/data/symbols.db")
(constant '*SK* "UOLL4")

(define (on-error)
  (print "error # " (last-error) " has occurred\n")
  (exit 1)
  )

(error-event 'on-error)

(define (get-symbols db query-fn)
  (if (file? db)
    (begin
      (sql3:open db)
      (local (r)
        (set 'r (apply query-fn (args)))
        (sql3:close)
        r))
    (throw-error "database not found")
    ))


(define (query)
  (local (symbol start end limit)
    (bind (args) true)
    (let (suf "")
      (if (> (int limit) 0) (set 'suf (format " LIMIT %d" limit)))
      (cond
       ((and (= start nil) (= end nil))
        (sql3:sql
         (append "SELECT D, O, H, L, C, V FROM symbols WHERE S = ? ORDER BY D" suf)
         (list symbol)))
       ((= end nil)
        (sql3:sql
         (append "SELECT D, O, H, L, C, V FROM symbols WHERE S = ? AND D >= ? ORDER BY D" suf)
         (list symbol start)))
       (true
        (sql3:sql
         (append "SELECT D, O, H, L, C, V FROM symbols WHERE S = ? AND D BETWEEN ? AND ? ORDER BY D" suf)
         (list symbol start end)))
       )
      )
    )
  )


(println
 (get-symbols *DB* 'query '(symbol *SK*) '(start "2011-06-01") '(end "2011-06-10") '(limit 10)))

(exit 0)

