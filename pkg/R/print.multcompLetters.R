"print.multcompLetters" <-
function(x, all=FALSE, ...){
  {
    if(all){
      class(x) <- NULL
      print(x, ...)
    }
    else 
      print(x$Letters, ...)
  }
  invisible(x$Letters)
}

