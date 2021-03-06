% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grepes_and.R
\name{\%and\%}
\alias{\%and\%}
\title{Get Intersection Set for Two Sets}
\usage{
a \%and\% b
}
\arguments{
\item{a}{can be numbers, characters, vectors, one dataframe, one matrix or one list.}

\item{b}{can be numbers, characters, vectors, one dataframe, one matrix or one list.}
}
\value{
intersection set
}
\description{
Get intersection set for two sets, which can be numbers, characters, vectors even dataframe, matrix or list.
}
\examples{
A <- c("a","b","c")
B <- c("a","b","c","d")
C <- c("a","e","h")
A \%and\% B
A \%and\% B \%and\% C
}
