(load "DDB.lsp")

(println "\n              (DOYLE DOWSON BEOWULF)")

(println "Doyle test  : " 
    (bayes-query (parse "adventures of sherlock holmes") 'DDB true)
    (bayes-query (parse "adventures of sherlock holmes") 'DDB))
(println "Downson test: "
    (bayes-query (parse "comedy of masks") 'DDB true)
    (bayes-query (parse "comedy of masks") 'DDB))
(println "Beowulf test: " 
    (bayes-query (parse "hrothgar and beowulf") 'DDB true)
    (bayes-query (parse "hrothgar and beowulf") 'DDB))

(println)
