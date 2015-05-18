#!/usr/bin/env newlisp

;; $ newlisp kmeans-sample.lsp > output.txt
;; gnuplot> plot "output.txt" index 0, "" index 1, ...

(define data-file
  ;"http://research.microsoft.com/en-us/um/people/cmbishop/prml/webdatasets/faithful.txt"
  ;"http://www.cs.cmu.edu/~dpelleg/kmeans/11class.unit.ds"
  "11class.unit.ds"
  )

(define (parse-file path)
  (let ((acc '()))
    (dolist (line (parse (read-file path) "\n"))
      (let ((xy (map float (0 2 (parse line)))))
        (when (and xy
                   (float? (xy 0))
                   (float? (xy 1)))
          (push xy acc -1))))
    acc))

(setq data (parse-file data-file))

(kmeans-train data 10 'K)
;(save "output.lsp" 'K 'data)

(dotimes (i (length K:clusters))
  (unless (empty? (K:clusters i))
    (println "# clusters[" i "]")
    (dolist (xy (select data (K:clusters i)))
      (println (format "%1.10f %1.10f" (xy 0) (xy 1))))
    (println)
    (println)))

(exit)

