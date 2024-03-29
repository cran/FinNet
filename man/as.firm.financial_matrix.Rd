% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04.FF_methods.R
\name{as.firm.financial_matrix}
\alias{as.firm.financial_matrix}
\title{Coerce a \code{financial_matrix} object into a list of \code{firm} objects}
\usage{
as.firm.financial_matrix(x, ...)
}
\arguments{
\item{x}{The object of class \code{financial_matrix} to be coerced}

\item{...}{Optional argument to the method}
}
\value{
A (usually named) list of \code{firm} objects the length of which equals the number of rows and columns of the provided \code{financial_matrix}
}
\description{
Un-exported function behind the \code{as.firm} method for objects of class \code{financial_matrix}
}
\author{
\enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
}
\keyword{internal}
