
CMIMselection <- function(X,y,kmax){
  n <- nrow(X)
  p <- ncol(X)
  s <- numeric(p)
  nu <- numeric(kmax)
  for(i in 1:p){
    s[i] <- mutinformation(y,X[,i])
  }
  for(k in 1:kmax){
    nu[k] <- which.max(s)
    for(i in 1:p){
      s[i] <- min(s[i],condinformation(y,X[,i],S=X[,nu[k]]))
    }
  }
}
s
CMIMselection(X,y,kmax=9)



library(mlbench)
data("BreastCancer")
obserwacje_bez_NA <- complete.cases(BreastCancer)
X <- BreastCancer[obserwacje_bez_NA , -c(1, 11)]
y <- BreastCancer[obserwacje_bez_NA , 11]

kmax=9
library(infotheo)

s
