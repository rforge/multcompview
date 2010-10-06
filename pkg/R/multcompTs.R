"multcompTs" <-
function(x, compare="<",
             threshold=0.05, sep="."){
##
## 1.  Covert to logical
##
  if(class(x)=="dist")x <- as.matrix(x)
  if(!is.logical(x))
    x <- do.call(compare, list(x, threshold))
##
## 2.  Convert to a symmetric matrix
##  
  x. <- vec2mat(x)
  if(any(diag(x.)))
    stop("Diag(x) must be or translate to FALSE;",
         " x = ", paste(x, collapse=", "))
##  
## 3.  Code insignificance as 0
##     and significance as (-1)  
##       
  k <- dim(x.)[1]
  x1 <- (1+x.)
  Dif <- array(c(0, -1)[x1], dim=c(k,k),
      dimnames=dimnames(x.))
  diag(Dif) <- 1
##
## 4.  To find recodes 0's as 1
##     then duplicate columns will
##     have inner product = k  
##
  dup.5 <- array(c(1, -1)[x1], dim=c(k,k),
                 dimnames=dimnames(x.))
# 
  Dup <- (crossprod(dup.5)==k)
##
## 5.  Look for dups only in the upper triangle
##     and drop all that are found
##  
  Dup[lower.tri(Dup, diag=TRUE)] <- FALSE
  dup.i <- which(colSums(Dup)>0)
  if(length(dup.i)>0){
    for(i in dup.i){
      j.i <- which(Dup[,i])[1]
      Dif[i, j.i] <- 1
      colNms <- dimnames(Dif)[[2]]
      j.i.Nm <- paste(colNms[c(j.i, i)], collapse=sep)
      dimnames(Dif)[[2]][j.i] <- j.i.Nm
    }
    Dif <- Dif[, -dup.i, drop=FALSE]
  }
##
## 6.  Done
##  
  class(Dif) <- "multcompTs"
  Dif
}

