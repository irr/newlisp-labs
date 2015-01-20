# R CMD BATCH --no-readline --no-timing --slave --restore --save '--args x=data.frame(Sepal.Length=5.9,Sepal.Width=3.0,Petal.Length=5.1,Petal.Width=1.8)' bayes.R 2>&1 && cat bayes.Rout && rm bayes.Rout

library(e1071)

init <- function() {
    data(iris)
    model <<- naiveBayes(Species ~ ., iris)
}

if (!exists(as.character(substitute(model)))) {
    init()
}

args=(commandArgs(TRUE))

if (length(args) > 0) {
    for (i in 1:length(args)) {
        eval(parse(text=args[[i]]))
    }
    print(as.character(predict(model, x)))
} else {
    print(as.character(predict(model, iris[150,])))
}
