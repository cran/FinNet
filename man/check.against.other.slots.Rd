% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04.FF_methods.R
\name{check.against.other.slots}
\alias{check.against.other.slots}
\title{Determine if all the non-\code{NA} slots of a \code{financial_matrix} object are of a given length}
\usage{
check.against.other.slots(x, number)
}
\arguments{
\item{x}{The object of class \code{financial_matrix} which to extract unique elements from}

\item{number}{The number against which to check the length of \code{x}'s non-\code{NA} slots}
}
\description{
Un-exported function called by the \code{ncol} and \code{nrow} methods for objects of class \code{financial_matrix}
}
\author{
\enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
}
\keyword{internal}
