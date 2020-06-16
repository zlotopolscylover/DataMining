
library(mlbench)
data("BreastCancer")
obserwacje_bez_NA <- complete.cases(BreastCancer)
X <- BreastCancer[obserwacje_bez_NA , -c(1, 11)]
y <- BreastCancer[obserwacje_bez_NA , 11]

#szybki
install.packages("infotheo")
library(infotheo)
CMIMselection1 <- function(X,y,kmax){
  stopifnot(exprs={anyNA(X)==FALSE;anyNA(y)==FALSE;is.data.frame(X);
    length(y)==nrow(X);ncol(X)>1;kmax<=ncol(X);is.finite(length(levels(y)))})
  n <- nrow(X)
  p <- ncol(X)
  nu <- numeric(kmax)
  ps <- numeric(p)
  m <- numeric(p)
  score <- numeric(kmax)
  ps <- unname(apply(X,2,function(x) mutinformation(y,x)))
  for(k in 1:kmax){
    nu[k] <- which.max(ps)
    s_gwiazdka <- 0
    for(i in 1:p){
      while(ps[i]>s_gwiazdka & m[i]< k-1){
        m[i] <- m[i] + 1
        ps[i] <-min(ps[i],condinformation(Y=y,X=X[,i],S=X[,nu[m[i]]]))
      }
      if(ps[i]>s_gwiazdka){
        s_gwiazdka <- ps[i]
        nu[k] <- i
      }
    }
    score[k] <- ps[nu[k]]
  }
  results <- list()
  results$S <- nu
  results$score <- score
  return(results)
}

CMIMsel <- CMIMselection1(X,y,kmax=9)

?stopifnot
CMIMsel$S
CMIMsel$score
is.finite(length(levels(y)))
length(levels(y))
