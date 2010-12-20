\name{multcompView-package}
\alias{multcompView-package}
\alias{multcompView}
\docType{package}
\title{
  Summarize multiple paired comparisons
}
\description{
  Convert a logical vector or a vector of
  p-values or a difference or distance matrix into a
  display identifying the pairs for which the
  differences were not significantly different or
  for which the difference exceeded a threshold.
}
\details{
\tabular{ll}{
Package: \tab multcompView\cr
Type: \tab Package\cr
Version: \tab 0.1-0\cr
Date: \tab 2006-08-06\cr
License: \tab GPL\cr
}
  Convert a logical vector or a vector of
  p-values or a difference or distance matrix into
  either a letter-based display using
  "multcompLetters" or a graphic roughly like a "T"
  using "multcompTs" to identify factor levels or
  similar groupings that are or are not
  significantly different.  Designed for use in
  conjunction with the output of functions like
  TukeyHSD, dist{stats}, simint, simtest, csimint,
  csimtest{multcomp}, friedmanmc,
  kruskalmc{pgirmess}.
}
\author{
  Spencer Graves and Hans-Peter Piepho with help
  from Sundar Dorai-Raj

  Maintainer:  Spencer Graves <spencer.graves@prodsyse.com>
}
\references{
  Piepho, Hans-Peter (2004) "An Algorithm for a
  Letter-Based Representation of All-Pairwise
  Comparisons", Journal of Computational and
  Graphical Statistics, 13(2)456-466.

  John R. Donaghue (2004) "Implementing Shaffer's
  multiple comparison procedure for a large number
  of groups", pp. 1-23 in Benjamini, Bretz and
  Sarkar (eds) Recent Developments in Multiple
  Comparison Procedures (Institute of Mathematical
  Statistics Lecture Notes-Monograph Series vol. 47)
}
\keyword{ package }
\keyword{ aplot }
\keyword{ dplot }
\keyword{ htest }
\examples{
dif3 <- c(FALSE, FALSE, TRUE)
names(dif3) <- c("a-b", "a-c", "b-c")
multcompTs(dif3)
multcompLetters(dif3)

library(MASS)
multcompBoxplot(Postwt~Treat, data=anorexia)
}

