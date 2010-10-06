"plotBoxes" <-
function(obj, at, width,
      horizontal, col, add, label.levels, 
      label.groups, orientation="", ...){
  if(orientation=="reverse")
    width[,] <- width[, 3:1]  
  lvl.rng <- range(at)
  gp.rng <- range(width)
  n <- dim(obj)[1]
  k <- dim(obj)[2]
# Convert to character to use with "switch"  
  ob. <- array(as.character(obj), dim=dim(obj),
               dimnames=dimnames(obj))
  {
    if(horizontal){
      plot(lvl.rng, gp.rng, type="n", xlab="",
          ylab="", bty="n", axes=FALSE, ...)
      for(i in 1:k){
        for(j in 1:n)
          switch(ob.[j,i],
             "-1"=next,
             "0" =rect(at[j, "bottom"], width[i, "bottom"],
               at[j, "top"], width[i, "top"],
               density=(-1), col=col[i], lty="blank"),
             "1" =polygon(
               x=at[j, c("bottom", "top", "center",
                      "bottom")],
               y=width[i, c("top", "top", "bottom",
                      "top")],
               density=(-1), col=col[i], lty="blank")
                 )
      }
#     Labels?
      if(!is.na(label.levels))
        text(at[, "center"],
             gp.rng[1]-label.levels*diff(gp.rng),
             dimnames(obj)[[1]])
      if(!is.na(label.groups))
        text(lvl.rng[1]-label.groups*diff(lvl.rng),
             width[, "center"], 
             dimnames(obj)[[2]])
    }
    else{
      plot(gp.rng, lvl.rng, type="n", xlab="",
          ylab="", bty="n", axes=FALSE, ...)
      for(i in 1:k){
        for(j in 1:n)
          switch(ob.[j,i],
             "-1"=next,
             "0" =rect(width[i, "bottom"],at[j, "bottom"], 
               width[i, "top"],at[j, "top"], 
               density=(-1), col=col[i], lty="blank"),
             "1" =polygon(
               x=width[i, c("top", "top", "bottom",
                      "top")],
               y=at[j, c("bottom", "top", "center",
                      "bottom")],
               density=(-1), col=col[i], lty="blank")
                 )
      }
#     Labels?
      if(!is.na(label.levels)){
        text(gp.rng[1]-label.levels*diff(gp.rng),
             at[, "center"],
             dimnames(obj)[[1]])
      }
      if(!is.na(label.groups))
        text(width[, "center"], 
             lvl.rng[1]-label.groups*diff(lvl.rng),
             dimnames(obj)[[2]])
    }
  }
  "Done"
}

