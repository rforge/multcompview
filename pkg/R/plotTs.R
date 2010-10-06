"plotTs" <-
function(obj, at, width,
      horizontal, col, add, lwd, label.levels, 
      label.groups, T.base, orientation="",
      ...){
  if(orientation=="reverse")
    width[,] <- width[, 3:1]  
  lvl.rng <- range(at)
  gp.rng <- range(width)
  n <- dim(obj)[1]
  k <- dim(obj)[2]
# compute the base of the Ts
  Tb <- T.base[1]
  a.b <- at[, "bottom"]
  a.c <- at[, "center"]
  a.t <- at[, "top"]
  at.5 <- cbind(a.c-Tb*(a.c-a.b),
                a.c+Tb*(a.t-a.c))
  {
    if(horizontal){
      plot(lvl.rng, gp.rng, type="n", xlab="",
          ylab="", bty="n", axes=FALSE, ...)
      for(i in 1:k){
        for(j in 1:n){
          if(obj[j,i]<0)next
#         Top of the "T"          
          rect(at[j, "bottom"], width[i, "center"],
               at[j, "top"], width[i, "top"],
               density=(-1), col=col[i], lty="blank") 
          if(obj[j,i]>0)
            rect(at.5[j, 1], width[i, "bottom"],
                 at.5[j, 2], width[i, "top"],
                 density=(-1), col=col[i], lty="blank")
#           Leg(s) of the "T"          
        }
#       Bridge across the top in case of gaps
        lines(range(at[obj[, i]>(-1), ]),
              rep(width[i, "top"],2),
              col=col[i], lwd=lwd)
      }
#     Labels?
      if(!is.na(label.levels)){
        text(at[, "center"],
             gp.rng[1]-label.levels*diff(gp.rng),
             dimnames(obj)[[1]])
      }
      if(!is.na(label.groups))
        text(lvl.rng[1]-label.groups*diff(lvl.rng),
             width[, "center"], 
             dimnames(obj)[[2]])
    }
    else{
      plot(gp.rng, lvl.rng, type="n", xlab="",
          ylab="", bty="n", axes=FALSE, ...)
      for(i in 1:k){
        for(j in 1:n){
          if(obj[j,i]<0)next
#         Top of the "T"          
          rect(width[i, "center"], at[j, "bottom"],
               width[i, "top"], at[j, "top"],
               density=(-1), col=col[i], lty="blank") 
          if(obj[j,i]>0)
            rect(width[i, "bottom"],at.5[j, 1], 
                 width[i, "top"],at.5[j, 2], 
                 density=(-1), col=col[i], lty="blank")
#           Leg(s) of the "T"          
        }
#       Bridge across the top in case of gaps
        lines(rep(width[i, "top"],2),
              range(at[obj[, i]>(-1), ]),
              col=col[i], lwd=lwd)
      }
#     Labels?
      if(!is.na(label.levels))
        text(gp.rng[1]-label.levels*diff(gp.rng),
             at[, "center"],
             dimnames(obj)[[1]])
      if(!is.na(label.groups))
        text(width[, "center"], 
             lvl.rng[1]-label.groups*diff(lvl.rng),
             dimnames(obj)[[2]])
    }
  }
  "Done"
}

