\name{multcompTs}
\alias{multcompTs}
\title{
  "T" depiction of undiffentiated classes
}
\description{
  Convert a logical vector or a vector of p-values
  or a correlation or distance matrix into a
  matrix with an associated visual display to show
  whether the differences between items exceed a
  threshold.  Designed for use with the output of
  functions like TukeyHSD, dist{stats}, simint,
  simtest, csimint, csimtest{multcomp}, friedmanmc,
  kruskalmc{pgirmess}.
}
\usage{
  multcompTs(x, compare="<", threshold=0.05,
                       sep=".")
}
\arguments{
  \item{x}{
    One of the following:
    (1) A square, symmetric matrix with row names.
    (2) A vector with hyphenated names, which identify
    individual items or factor levels after
    "strsplit(..., '-')".
    (3) An object of class "dist".

    If x (or x[1]) is not already of class "logical",
    it is replaced with do.call(compare, list(x,
    threshold)), which by default converts numbers
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
  \item{sep}{
    Concatonation character for names of objects with
    identical similarity / dissimilarity patterns.
    The output of multcompTs is matrix for which the
    number of rows = (number of columns + number of
    uses of the "sep" character).
  }
}
\details{
  Produces a matrix of class "multcompTs",
  describing the "undifferentiated classes"
  that identify the other factor levels or items
  that are not distinct or not significantly
  different from the "base" of the "T";  if two
  or more levels have the same pattern of
  significant differences, the two are combined
  into one "T" with two "bases".  The resulting
  T's are similar to the "undifferentiated
  classes" discussed by Donaghue (2004).
}
\value{
  An object of class "multcompTs", which is a matrix
  of values {-1, 0, 1}, with one row for each level
  compared and one column for each "T", read as
  follows:
  1 = base of the "T" represented by that column,
  0 = level(s) not significantly different, and
  (-1) = leves(s) significantly different.
  If two or more levels have the same pattern of
  significant and insignificant differences, they
  are combined into a single column that can be
  represented by a "T" with multiple bases.  The
  column name will be a character string concatonating
  all row names with "1" in that column separated by
  the "sep" character.  Thus, the matrix should have
  as many 1's as it has rows.  Also, the lower
  triangular portion should have as many "-1's" as
  there are "TRUE" (e.g., significant) differences
  among the comparisons.
}
\references{
  John R. Donaghue (2004) "Implementing Shaffer's
  multiple comparison procedure for a large number of
  groups", pp. 1-23 in Benjamini, Bretz and Sarkar
  (eds) Recent Developments in Multiple Comparison
  Procedures (Institute of Mathematical Statistics
  Lecture Notes-Monograph Series vol. 47)

  Spencer Graves and Hans-Peter Piepho (2006) "Simple
  Visualizations of Paired Comparisons",
  \url{dir(system.file('doc', package='multcompView'),
    pattern='\\.pdf$', full.name=TRUE)}
}
\author{ Spencer Graves and Hans-Peter Piepho }
\seealso{
  \code{\link{multcompBoxplot}}
  \code{\link{multcompLetters}}
  \code{\link{plot.multcompTs}}
  \code{\link{vec2mat}}
  \code{\link{dist}}
}
\examples{
##
## 0.  Conference presentation comparing Ts and Letters
##
dir(system.file('doc', package='multcompView'),
    pattern='\\\\.pdf$', full.name=TRUE)

##
## 1.  logical vector indicating different pairs
##
dif3 <- c(FALSE, FALSE, TRUE)
names(dif3) <- c("a-b", "a-c", "b-c")
multcompTs(dif3)

##
## 2.  numeric vector indicating statistical significance
##
dif4 <- c(.01, .02, .03, 1)
names(dif4) <- c("a-b", "a-c", "b-d", "a-d")
(diff4.T <- multcompTs(dif4))
plot(diff4.T)

##
## 3.  Distance matrix
##
dJudge <- dist(USJudgeRatings)
dJt <- multcompTs(dJudge, compare='>', threshold = median(dJudge))
# comparison of 43 judges;  compact but undecipherable:
plot(dJt, cex.axis=.5)

x <- array(1:9, dim=c(3,3),
   dimnames=list(LETTERS[1:3], NULL) )
d3 <- dist(x)
dxTs <- multcompTs(d3, compare=">", threshold=2)
plot(dxTs)

d3d <- dist(x, diag=TRUE)
dxdTs <- multcompTs(d3d, compare=">", threshold=2)

\dontshow{stopifnot(}
all.equal(dxTs, dxdTs)
\dontshow{)}

d3u <- dist(x, upper=TRUE)
dxuTs <- multcompTs(d3d, compare=">", threshold=2)

\dontshow{stopifnot(}
all.equal(dxTs, dxuTs)
\dontshow{)}

##
## 4.  cor matrix
##
set.seed(4)
x100 <- matrix(rnorm(100), ncol=5,
               dimnames=list(NULL, LETTERS[1:5]) )
cx <- cor(x100)
cxTs <- multcompTs(abs(cx), threshold=.3)
plot(cxTs)

}
\keyword{ dplot }
