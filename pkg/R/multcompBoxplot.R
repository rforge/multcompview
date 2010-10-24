"multcompBoxplot" <-
function(formula, data, horizontal=TRUE,
  compFn="TukeyHSD", sortFn="mean",
  decreasing=TRUE, plotList=list(
    boxplot=list(fig=c(0, 0.75, 0, 1)),
    multcompTs=list(fig=c(0.7, 0.85, 0, 1)),
    multcompLetters=list(fig=c(0.87, 0.97, 0.03, 0.98),
      fontsize=20, fontface="bold"))){
##
## 1.  if(!horizontal)swap plotList$...$fig...
##
  n.mc <- length(plotList)
  if(!horizontal)
    for(i in 1:n.mc)
      if("fig" %in% names(plotList[[i]]))
        plotList[[i]]$fig <-
          plotList[[i]]$fig[c(3,4,1,2)]
##
## 2.  Sort the levels of 'z' in the formula
##
  if(!is.null(sortFn) && (is.function(sortFn) ||
       ((!is.na(sortFn)) && is.character(sortFn)))){
    fm <- as.character(formula)
    data <- data[, fm[-1]]
    y.z <- tapply(data[, fm[2]], data[, fm[3]],
           function(x)do.call(sortFn, list(x=x)))
    oz <- order(y.z, decreasing=decreasing)
    data[, fm[3]] <- factor(data[, fm[3]],
           levels=names(y.z)[oz])
  }
##
## 3.  Create the desired boxplots
##
  bpArgs <- plotList$boxplot
  if("fig" %in% names(bpArgs)){
    op <- par(fig=bpArgs$fig)
    on.exit(par(op))
    bpArgs$fig <- NULL
  }
#
  bpArgNames <- names(bpArgs)
  nArgs.bp <- length(bpArgNames)
  bpArg <- vector(3+nArgs.bp, mode="list")
  names(bpArg) <- c("formula", "data",
       "horizontal", bpArgNames)
  bpArg[[1]] <- formula
  bpArg[[2]] <- data
  bpArg[[3]] <- horizontal
  if(nArgs.bp>0)for(i in 1:nArgs.bp)
    bpArg[[3+i]] <- bpArgs[[i]]
  bp <- do.call("boxplot", bpArg)
# Create list "out" and save this.
  out <- vector(n.mc+1, mode="list")
  plotNames <- names(plotList)
  names(out) <- c(plotNames[1], "compFn", plotNames[-1])
  out[[1]] <- bp
  par(op)
##
## 4.  Call "compFn" for the other portions of the
##     display.
##
  Fn <- compFn
  if(compFn=="TukeyHSD"){
    TukeyHSD. <- function(formula, data){
      TukeyHSD(aov(formula, data))[[1]][, "p adj"]
    }
    Fn <- "TukeyHSD."
  }
  fnValue <- do.call(Fn,
          list(formula=formula, data=data))
  Fn.v0 <- vec2mat(fnValue)
  Lvls <- bp$names
  if(horizontal)Lvls <- rev(Lvls)
  FnValue <- Fn.v0[Lvls, Lvls]
  out[["compFn"]] <- fnValue
##
## 5.  Process the remaining components of plotList
##
  if(n.mc>1){
    mar2 <- (horizontal*c(5, 0, 4, 0)+
           (!horizontal)*c(0, 4, 0, 2)+0.1)
    mcNames <- plotNames[-1]
#    plotNms <- paste("plot", mcNames, sep=".")
    if("multcompTs" %in% mcNames)
      mcTs <- multcompTs(FnValue)
    if("multcompLetters" %in% mcNames)
      mcLtrs <- multcompLetters(FnValue)
#   Create the desired multcompViews
    for(i in 1:(n.mc-1)){
      pL.i <- plotList[[i+1]]
      {
        if("mar" %in% names(pL.i)){
          mar <- pL.i$mar
          pL.i$mar <- NULL
        }
        else
          mar <- mar2
      }
      op.i <- par(mar=mar)
      on.exit(par(op.i))
      names.i <- names(pL.i)
      nm.i <- length(names.i)
      args.i <- vector(3+nm.i, mode="list")
      names(args.i) <- c("x", "horizontal",
                         "add", names.i)
      args.i[[1]] <- {
        if(mcNames[i]=="multcompTs")
          mcTs else mcLtrs
      }
      args.i[[2]] <- !horizontal
      args.i[[3]] <- TRUE
      if(nm.i>0)for(ji in 1:nm.i)
        args.i[[3+ji]] <- pL.i[[ji]]
      out[[2+i]] <- do.call(plot, args.i)
      par(op.i)
    }
  }
  invisible(out)
}

