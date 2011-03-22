\name{multcompLetters}
\alias{multcompLetters}
\title{
  Letter summary of similarities and differences
}
\description{
  Convert a logical vector or a vector of
  p-values or a correlation or distance matrix into
  a character-based display in which common
  characters identify levels or groups that are not
  significantly different.  Designed for use with
  the output of functions like TukeyHSD,
  dist{stats}, simint, simtest, csimint,
  csimtest{multcomp}, friedmanmc,
  kruskalmc{pgirmess}.
}
\usage{
multcompLetters(x, compare="<", threshold=0.05,
                Letters=c(letters, LETTERS, "."), 
                reversed = FALSE)
}
\arguments{
  \item{x}{
    One of the following:
    (1) A square, symmetric matrix with row names.
    (2) A vector with hyphenated names, which identify
    individual items or factor levels after "strsplit".
    (3) An object of class "dist".
    If x (or x[1]) is not already of class "logical",
    it is replaced with do.call(compare, list(x,
    threshold)),  which by default converts numbers
    (typically p-values) less than 0.05 to TRUE and
    everything else to FALSE.  If x is a matrix, its
    diagonal must be or must convert to FALSE.
  }
  \item{compare}{
    function or binary operator;  not
    used if class(x) is "logical".
  }
  \item{threshold}{
    Second (reference) argument to "compare".
  }
  \item{Letters}{
    Vector of distinct characters (or character
    strings) used to connect levels that are not
    significantly different.  They should be
    recogizable when concatonated.  The last
    element of "Letters" is used as a prefix for
    a reuse of "Letters" if more are needed than
    are provided.  For example, with the default
    "Letters", if 53 distinct connection colums are
    required, they will be "a", ..., "z", "A", ...,
    "Z", and ".a".  If 54 are required, the last
    one will be ".b".  If 105 are required, the
    last one will be "..a", etc.  (If the algorithm
    generates that many distinct groups, the display
    may be too busy to be useful, but the algorithm
    shouldn't break.)
  }
  \item{reversed}{
  A logical value indicating whether the order of 
  the order of the letters should be reversed. 
  Defaults to FALSE.
  }
}
\details{
  Produces a "Letter-Based Representation of All-
  Pairwise Comparisons" as described by Piepho
  (2004).  (The present algorithm does NOT perform
  his "sweeping" step.)
}
\value{
  An object of class 'multcompLetters', which is
  a list with the following components:
  \item{Letters }{character vector with names = the
    names of the levels or groups compared and with
    values = character strings in which common
    values of the function argument "Letters"
    identify levels or groups that are not
    significantly different (or more precisely
    for which the corresponding element of "x"
    was FALSE or was converted to FALSE by
    "compare").
    }
    \item{monospacedLetters }{
      Same as "Letters" but with spaces so the
      individual grouping letters will line up
      with a monspaced type font.
    }
  \item{LetterMatrix }{Logical matrix with one row for
    each level compared and one column for each
    "Letter" in the "letter-based representation".
    The output component "Letters" is obtained by
    concatonating the column names of all columns
    with TRUE in that row.
    }
}
\references{
  Piepho, Hans-Peter (2004) "An Algorithm for a Letter-Based
  Representation of All-Pairwise Comparisons", Journal of
  Computational and Graphical Statistics, 13(2)456-466.
}
\author{ Spencer Graves and Hans-Peter Piepho }
\seealso{
  \code{\link{multcompBoxplot}}
  \code{\link{plot.multcompLetters}}
  \code{\link{print.multcompLetters}}
  \code{\link{multcompTs}}
  \code{\link{vec2mat}}
}
\examples{
##
## 1.  a logical vector indicating signficant differences
##
dif3 <- c(FALSE, FALSE, TRUE)
names(dif3) <- c("A-B", "A-C", "B-C")
dif3L <- multcompLetters(dif3)
dif3L
print(dif3L)
print(dif3L, TRUE)

##
## 2.  numeric vector indicating statistical significance
##
dif4 <- c(.01, .02, .03, 1)
names(dif4) <- c("a-b", "a-c", "b-d", "a-d")
(diff4.T <- multcompLetters(dif4))

(dif4.L1 <- multcompLetters(dif4,
       Letters=c("*", ".")))
# "Letters" can be any character strings,
# but they should be recognizable when
# concatonated.

##
## 3.  distance matrix
##
dJudge <- dist(USJudgeRatings)
dJl <- multcompLetters(dJudge, compare='>', threshold = median(dJudge))
# comparison of 43 judges;  compact but undecipherable:
dJl

x <- array(1:9, dim=c(3,3),
   dimnames=list(LETTERS[1:3], NULL) )
d3 <- dist(x)
dxLtrs <- multcompLetters(d3, compare=">", threshold=2)

d3d <- dist(x, diag=TRUE)
dxdLtrs <- multcompLetters(d3d, compare=">", threshold=2)

\dontshow{stopifnot(}
all.equal(dxLtrs, dxdLtrs)
\dontshow{)}

d3u <- dist(x, upper=TRUE)
dxuLtrs <- multcompLetters(d3d, compare=">", threshold=2)

\dontshow{stopifnot(}
all.equal(dxLtrs, dxuLtrs)
\dontshow{)}

##
## 4.  cor matrix
##
set.seed(4)
x100 <- matrix(rnorm(100), ncol=5,
               dimnames=list(NULL, LETTERS[1:5]) )
cx <- cor(x100)
cxLtrs <- multcompLetters(abs(cx), threshold=.3)


##
##5. reversed
##

dif3 <- c(FALSE, FALSE, TRUE)
names(dif3) <- c("A-B", "A-C", "B-C")
dif3L <- multcompLetters(dif3)
dif3L.R <- multcompLetters(dif3, rev = TRUE)
dif3L
dif3L.R


}
\keyword{ dplot }