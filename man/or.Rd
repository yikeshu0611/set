% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/a_or.R
\name{or}
\alias{or}
\title{Get Union Set for Sets}
\usage{
or(...)
}
\arguments{
\item{...}{see argument x in \code{\link[set]{toVector}}}
}
\value{
union elements
}
\description{
Get union set for sets.
}
\examples{
A <- c("a","b","c")
B <- c("a","b","c","d")
C <- c("a","e","h")
or(A, B)
or(A, B, C)
}
