% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comp.R
\name{comp}
\alias{comp}
\title{Get Complementary Elements}
\usage{
comp(a, b)
}
\arguments{
\item{a}{must be a subset of b, which can be numbers, characters, vectors, one dataframe, one matrix or one list.}

\item{b}{can be numbers, characters, vectors, one dataframe, one matrix or one list.}
}
\value{
complementary elements in data set b
}
\description{
Get complementary elements
}
\examples{
A <- c("a","b","c")
B <- c("a","b","c","d")
comp(A, B)
}
