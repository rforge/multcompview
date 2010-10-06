\name{print.multcompLetters}
\alias{print.multcompLetters}
\title{ print a multcompLetters object }
\description{
  print method for an object of class
  'multcompLetters'.  
}
\usage{
\method{print}{multcompLetters}(x, all=FALSE, ...)
}
\arguments{
  \item{x}{
    an object of class 'multcompLetters'
  }
  \item{all}{
    FALSE to print only the character vector
    representations of the 'multcompLetters'
    comparison summary;  
    TRUE to print also the matrix
    representation.  
  }
  \item{...}{
    Other optional print parameters as described 
    on the \code{\link{print}} help page.  
  }
}
\details{
  Prints only the Letters component of the
  'multcompLetters' list unless all=TRUE.  
}
\value{
  x\$Letters = the named, character vector
  representation of the 'multcompLetters'
  evaluation of the distance relationships
}
\author{ Spencer Graves }
\seealso{
  \code{\link{multcompLetters}} 
}
\examples{
dif3 <- c(FALSE, FALSE, TRUE)
names(dif3) <- c("A-B", "A-C", "B-C")
dif3L <- multcompLetters(dif3)
dif3L
print(dif3L)
print(dif3L, TRUE)
}
\keyword{ dplot }
