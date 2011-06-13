"extract_p" <- function(x) {
  UseMethod("extract_p")
}

extract_p.default <- function(x){
  ans <- x[ ,"p adj"]
  #To be sure that names are kept
  names(ans) <- rownames(x)
  ans
}

"extract_p.TukeyHSD" <- function(x) {
   x <- lapply(x, extract_p.default)
   x
}

