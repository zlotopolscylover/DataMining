# szybki

# Ładowanie wymaganych pakietów

if(!require(infotheo)){
  install.packages("infotheo")
  library(infotheo) 
}

# rozwiązanie

CMIMselection <- function(X, y, kmax){
  stopifnot(exprs={anyNA(X)==FALSE;anyNA(y)==FALSE;is.data.frame(X);
    length(y)==nrow(X);ncol(X)>1;kmax<=ncol(X);kmax%%1==0;kmax>0;
    is.finite(length(levels(y)))})
  n <- nrow(X)
  p <- ncol(X)
  S <- numeric(kmax)
  ps <- numeric(p)
  m <- numeric(p)
  score <- numeric(kmax)
  ps <- unname(apply(X,2,function(x) mutinformation(y,x)))
  for(k in 1:kmax){
    S[k] <- which.max(ps)
    s <- 0
    for(i in 1:p){
      while(ps[i]>s & m[i]< k-1){
        m[i] <- m[i] + 1
        ps[i] <-min(ps[i],condinformation(Y=y,X=X[,i],S=X[,S[m[i]]]))
      }
      if(ps[i]>s){
        s <- ps[i]
        S[k] <- i
      }
    }
    score[k] <- ps[S[k]]
  }
  return(list(S = S, score = score))
} 

