\name{vec2mat2}
\alias{vec2mat2}
\title{
  Convert a vector of hyphenated names into
  a character matrix.  
}
\description{
  Convert a vector of hyphenated names into
  a character matrix with 2 columns containing 
  the names split in each row.  
}
\usage{
vec2mat2(x, sep="-")
}
\arguments{
  \item{x}{
    Vector of hyphenated names 
  }
  \item{sep}{
    "strsplit" character to apply to names(x).  
  }
}
\details{
  If each element of x does not contain exactly 1
  "sep" character, an error is issued.  
}
\value{
  A character matrix with rownames = x and with the
  character string preceeding the "sep" character in the
  first column and the character string following the
  "sep" character in the second column.  
}
\author{ Spencer Graves  }
\seealso{
  \code{\link{vec2mat}}
  \code{\link{multcompLetters}}
}
\examples{
vec2mat2(c("a-b", "a-c", "b-c"))

vec2mat2(c("a-b", "b-a"))

\dontshow{
(tst3 <- substring(try(
 vec2mat2(c("a", "b-a", "b-c"))), 1, 20)
=="Error in vec2mat2(c(")
# Error:  name without a sep character 

(tst4 <- substring(try(
 vec2mat2(c("a-c", "b-a", "b-c-d"))), 1, 20)
=="Error in vec2mat2(c(")
# Error:  multiple hyphens (sep characters)

} 
}
\keyword{ manip }
\keyword{ array }
