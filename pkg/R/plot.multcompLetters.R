"plot.multcompLetters" <-
function(x,
   horizontal=FALSE, col=1:6,
   type=c("Letters", "boxes"), add=FALSE,
   at, width, fig=c(0, 1, 0, 1),
   label.levels=if(add)NA else 0.05, 
   label.groups=NA, ...){
##
## 1.  Get row and column names
##
  obj <- deparse(substitute(x))
  objMat <- x$LetterMatrix
  lvls <- dimnames(objMat)[[1]]
  gps <- dimnames(objMat)[[2]]
  n <- length(lvls)
  k <- length(gps)
  if((n==0) | (k==0))
    stop("dimnames required for ", obj)
  col <- rep(col, len=k)
##
## 2.  Get plotting positions for lvls and gps
##
  At <- function(x, lvls, d){
    N <- length(lvls)
    if(length(x)<=N)      
      x <- t(outer(c(-0.4, 0, 0.4), x, "+"))
    else{
      x <- apply(x, 2, sort)
      if(length(x)<(3*N))
        x <- cbind(x[, 1], mean(x, 2, mean),
                   x[, 2])
    }
    dimnames(x) <- list(lvls,
         c("bottom", "center", "top"))
    x
  }
#
  at.list <- FALSE
  missW <- missing(width)
  {
    if(missing(at)){
      at <-{
        if(horizontal) 1:n else n:1
      }
    }
    else 
      if(is.list(at)){
        at.list <- TRUE
        if(is.null(names(at)))
          names(at) <- c("at", "width")
        width <- at$width
        at <- at$at
      }
  }
#
  if(!at.list){
    if(missW){
      width <- {
        if(horizontal) k:1 else 1:k
      }
    }      
    width <- At(width, gps)
    at <- At(at, lvls)
  }
## 
## 3.  Adjust par as required 
##
  op. <- par()
  parList <- list(fig=fig, xpd=NA, new=add)
  if(add)parList$mar <- (c(5, 0, 4, 0)+0.1)
#  
  dots <- list(...)
  dNames <- names(dots)
  nDots <- length(dNames)
  if(nDots>0)for(i in 1:nDots)
    if(dNames[i] %in% names(op.)){
      parList[[dNames[i]]] <- dots[[dNames[i]]]
      dots[[dNames[i]]] <- NULL
    }
  op <- do.call("par", parList)
  on.exit(par(op))
## 
## 4.  organize plot args 
##
  plotL <- (match.arg(type)=="Letters")
  pArgs <- names(dots)
  k.pArgs <- length(pArgs)
  stdArgs <- c("obj", "at", "width",
       "horizontal", "col", "add",
       "label.levels")
  ks <- length(stdArgs)
  argNames <- {
    if(plotL) c(stdArgs, pArgs)
    else c(stdArgs, "label.groups", pArgs)
  }
  plotArgs <- vector(length(argNames), mode="list")
  names(plotArgs) <- argNames
  plotArgs$obj <- {
    if(plotL) objMat
    else (objMat-1)
  }
  plotArgs$at <- at
  plotArgs$width <- width
  plotArgs$horizontal <- horizontal
  plotArgs$col <- col
  plotArgs$add <- add
  plotArgs$label.levels <- label.levels
#  
  if(!plotL)
    plotArgs$label.groups <- label.groups
  if(k.pArgs>0)for(i. in 1:k.pArgs)
    plotArgs[[pArgs[i.]]] <- dots[[i.]]
##
## 5.  Plot
##
  do.call(c("plotBoxes", "plotLetters")[1+plotL],
          plotArgs)
##
## 6.  Done
##
  list(at=at, width=width)
}

