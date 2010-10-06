"plotLetters" <- 
function(obj, at, horizontal, col, add,
    label.levels, font.family="mono",
    fig=par("fig"), mar=par("mar"), ...){
##
## 1.  Modify "par"
##
  library(grid)
  if(!add)grid.newpage()
#  
  op <- par(family=font.family)
  on.exit(par(op))  
##
## 2.  Set up 'grid' graphics 
##  
# Use 'grid' graphics for 2 reasons:
#   (1) I couldn't figure out how to get 
#       consistent alignment with overlays
#       using 'text'
#   (2) 'grid' will compute the size 
#       of characters, so I can use that
#       to determine plotting positions.  
  vpFig <- viewport(name="vp.fig")
  pushViewport(vpFig)
#  
  angle <- 90*horizontal
  lvl.rng <- range(at[, "center"])
##
## 3.  Compute figure and plot regions
##
  n <- dim(obj)[1]
  k <- dim(obj)[2]
# Figure region 
  W.fig <- fig[(1:2)+2*horizontal]
  At.fig <- fig[(3:4)-2*horizontal]
# Figure margins
  At.mar <- mar[c(1, 3)+horizontal]
  W.mar <- mar[c(2, 4)-horizontal]
# Plot region
  At.rng <- (At.fig+c(1, -1)*At.mar/37)
  W.rng <- (W.fig+c(1, -1)*W.mar/37)
##
## 4.  Rescale "at" to Grid's "npc" 
##     = "normalized parent coordinates"
##     within fig and mar
##  
  at.rng <- (range(at[, "center"])+c(-0.5, 0.5))
  d.at <- diff(at.rng)
  d.At <- diff(At.rng)
  At <- (At.rng[1]+d.At*(
              at[, "center"]-at.rng[1])/d.at)
#  At.npc <- unit(At, "npc")
#  At.nat <- unit(At, "native")
##
## 5.  Convert "Letters" to grobs =
##     Grid "graphics objects" 
##  
  n <- dim(obj)[1]
  k <- dim(obj)[2]  
  Ltrs <- dimnames(obj)[[2]]
  k1 <- k+1
  LtrsM <- c(Ltrs, "M")
  LtrsM. <- c(Ltrs, "Ref.M.")
  gLtrs <- vector(k1, mode="list")
  names(gLtrs) <- LtrsM
#  npc0 <- unit(0, "npc")
  nat0 <- unit(0, "native")
  for(j in 1:k1)
    gLtrs[[j]] <- textGrob(LtrsM[j], rot=angle,x=nat0,
       y=nat0, name=LtrsM.[j], gp=gpar(col=col[j], ...))
#          name=LtrsM.[j], gp=gpar(...))
##
## 6.  Compute character widths
##
  wLtrs <- rep(NA, k1)
  names(wLtrs) <- LtrsM
  {
    if(horizontal)
      for(j in 1:k1){
        gH.j <- grobHeight(gLtrs[[j]])
        wLtrs[j] <- convertHeight(gH.j, "native",
                       valueOnly=TRUE)
      }
    else
      for(j in 1:k1){
        gW.j <- grobWidth(gLtrs[[j]])
        wLtrs[j] <- convertWidth(gW.j, "npc",
                       valueOnly=TRUE)
      }
  }
##
## 7.  Rescale to W.rng
##
#     7.1.  maxX = max width including Ref.M.       
  maxW <- max(wLtrs)
#     7.2.  w.Ltrs = adj. width excl. Ref.M.      
  w.Ltrs <- 0.5*(wLtrs[-k1]+maxW)
#     7.3.  Convert to a scale in "npc"      
  sumW <- cumsum(w.Ltrs)
#     7.4.  width(in "npc") = adjustment to W.rng
  w0 <- mean(W.rng)
  W <- (w0+sumW-mean(range(sumW)))
#  W.npc <- unit(W, "npc")
#  W.nat <- unit(W, "native")
##
## 8.  Plot
##  
  {
    if(horizontal)
      for(j in 1:k){
        n.j <- sum(obj[, j])
        Ltr.j <- rep(Ltrs[j], n.j)
        W.j <- rep(W[j], n.j)
        At.j <- At[obj[, j]]
        grid.text(Ltr.j, At.j, W.j, rot=90,
                  gp=gpar(col=col[j], ...))
      }
    else
      for(j in 1:k){
        n.j <- sum(obj[, j])
        Ltr.j <- rep(Ltrs[j], n.j)
        W.j <- rep(W[j], n.j)
        At.j <- At[obj[, j]]
        grid.text(Ltr.j, W.j, At.j,
                  gp=gpar(col=col[j], ...))
      }
  }
##
## 9.  Label the levels?    
##
  if(!is.na(label.levels)){
    lvls <- dimnames(obj)[[1]]
#   W.mar = (W.rng[1]- 0.5*dW - label.levels)    
    W.mar <- (W[1]-0.5*wLtrs[1]-label.levels)
    W.n <- rep(W.mar, n)
    {
      if(horizontal)
        grid.text(lvls, At, W.n, rot=90)
      else
        grid.text(lvls, W.n, At)
    }
  }
##
## 10.  Clean up and quit.
##      
  popViewport()
#  
  "Done"
}

