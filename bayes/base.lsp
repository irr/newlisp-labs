;; from Project Gutenberg: http://www.gutenberg.org/catalog/

(module "zlib.lsp")

(bayes-train (parse (lower-case (zlib:gz-read-file "Doyle.txt.gz")) "[^a-z]+" 0) '() '() 'DDB)
(bayes-train '() (parse (lower-case (zlib:gz-read-file "Dowson.txt.gz")) "[^a-z]+" 0) '() 'DDB)
(bayes-train '() '() (parse (lower-case (zlib:gz-read-file "Beowulf.txt.gz")) "[^a-z]+" 0) 'DDB)

(save "DDB.lsp" 'DDB)
(exit 0)
