% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/a_not.R
\name{not}
\alias{not}
\title{Get Elements only Existed in Dataset a}
\usage{
not(...)
}
\arguments{
\item{...}{see argument x in \code{\link[set]{toVector}}}
}
\value{
elements only existed in dataset a
}
\description{
Get elements only existed in dataset a.
}
\examples{
A <- c("a","b","c")
B <- c("a","b","c","d")
not(B, A)
}
