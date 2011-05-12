"extract_p" <- function(x) {
  UseMethod("extract_p")
}

extract_p.default <- function(x){
  ans <- drop(as.matrix(x[,"p adj", drop = FALSE]))
  ans
}

"extract_p.TukeyHSD" <- function(x) {
   x <- lapply(x, extract_p.default)
   x
}

