% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04.FF_methods.R
\name{show,financial_matrix-method}
\alias{show,financial_matrix-method}
\title{Show information on a \code{financial_matrix}}
\usage{
\S4method{show}{financial_matrix}(object)
}
\arguments{
\item{object}{The \code{financial_matrix} object to show}
}
\value{
No return value, called to print to the console information about the \code{financial_matrix} object:
\itemize{
  \item type of relation detail (common ownership, board interlocks, or both),
  \item Total capitalisation of the firms present (if known), broke down by currency (if more than one);
  \item Average total revenues (if known), broke down by currency (if more than one)
  \item A print-out of the matrix, up to six columns by six rows (i.e., 36 cells). An additional row of text is printed out to inform on the truncation of the matrix displayed
}
}
\description{
Show method for the objects of class \code{financial_matrix}
}
\author{
\enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
}
\keyword{internal}
