\name{vec2mat}
\alias{vec2mat}
\title{
  Convert a vector with hyphenated names into
  a matrix.  
}
\description{
  Convert a vector with hypehnated names into
  a symmetric matrix with names obtained from 
  vect2mat2(names(x)).  
}
\usage{
  vec2mat(x, sep="-")
}
\arguments{
  \item{x}{
    Either (1) a vector with hyphenated names indicating
    pairs of factor levels, groups or items that are and
    are not significantly different or (2) a matrix
    indicating same.  If x is already a matrix, it is
    checked for symmetry.  NAs are not allowed.  
  }
  \item{sep}{
    "strsplit" character to apply to names(x).  
  }
}
\details{
  x must have names each of which contains exactly
  one hyphen;  if not, vec2mat issues an error message.
  If the same comparison is present multiple times,
  the last value is used; no check is made for
  duplicates.  
}
\value{
  A symmetrix matrix of the same class as the input
  with names obtained from unique(strsplit(names(x))).  
  All nonspecified elements will be 1 if class(x) is
  numeric, FALSE if logical and "" if character.  Used
  by the functions 'multcompLetters' and 'multcompTs'.  
}
\author{ Spencer Graves  }
\seealso{
  \code{\link{multcompLetters}}
  \code{\link{multcompTs}}
}
\examples{
dif3 <- c(FALSE, FALSE, TRUE)
names(dif3) <- c("a-b", "a-c", "b-c")
vec2mat(dif3)

dif3. <- 1:3
names(dif3.) <- c("a-b", "a-c", "b-c")
vec2mat(dif3.)

dif.ch <- c("this",'is','it')
names(dif.ch) <- c("a-b", "a-c", "b-c")
vec2mat(dif.ch)

vec2mat(array(1, dim=c(2,2)))
\dontshow{
  try(vec2mat(array(1:24, dim=2:4)))# must be 2-d
  try(vec2mat(array(1:6, dim=2:3)))# must be square
  try(vec2mat(array(1:4, dim=c(2,2))))# must be symmetric
  try(vec2mat(array(1, dim=c(2,2)))) # diag should be 0
  try(vec2mat(array(TRUE, dim=c(2,2)))) # diag should be FALSE
  try(vec2mat(array("a", dim=c(2,2)))) # diag should be ""

  try(vec2mat(c(1:3, NA))) # NAs not allowed 
  try(vec2mat(1:3))# Error:  No names

  errVec2 <- 1:3
  names(errVec2) <- c("a", "b-a", "b-c")
  try(vec2mat(errVec2))# Error:  missing hyphen (sep character)

  errVec3 <- 1:3
  names(errVec3) <- c("a-c", "b-a", "b-c-d")
  try(vec2mat(errVec3))
# Error:  multiple hyphens (sep characters)

  dif4 <- 1:4
  names(dif4) <- c("a-b", "a-c", "b-c", "b-a")
# Both "b-a" and "a-b" specified;
# use the latest.   
  vec2mat(dif4)
} 
}
\keyword{ manip }
\keyword{ array }
