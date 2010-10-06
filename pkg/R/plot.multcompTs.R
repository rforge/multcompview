"plot.multcompTs" <-
function(x,
   horizontal=FALSE, col=1:6,
   type=c("Ts", "boxes"), 
   orientation=c("standard", "reverse"),
   add=FALSE, at, width, fig=c(0, 1, 0, 1),
   lwd=3, label.levels=if(add)NA else 0.05,
   label.groups=NA, T.base=0.4, ...){
##
## 1.  Get row and column names
##
  obj <- deparse(substitute(x))
  lvls <- dimnames(x)[[1]]
  gps <- dimnames(x)[[2]]
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
  or. <- match.arg(orientation)
## 
## 3.  Set up the plot 
##
  op <- {
    if(add)par(fig=fig, xpd=NA, new=TRUE)
    else par(fig=fig, xpd=NA)
  }
  on.exit(par(op))
#
  {
    if(match.arg(type)=="Ts")
      plotTs(obj=x, at=at, width=width, 
         horizontal=horizontal, col=col,
         add=add, lwd=lwd,
         label.levels=label.levels, 
         label.groups=label.groups,
         T.base=T.base, orientation=or.,
         ...)
    else
      plotBoxes(obj=x, at=at, width=width, 
         horizontal=horizontal, col=col,
         add=add, label.levels=label.levels, 
         label.groups=label.groups,
         orientation=or., ...)
  }
  list(at=at, width=width)
}

