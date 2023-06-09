% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01.firm_methods.R
\name{print,firm-method}
\alias{print,firm-method}
\title{Print information on a class \code{firm} object}
\usage{
\S4method{print}{firm}(x)
}
\arguments{
\item{x}{The \code{firm} object to show}
}
\value{
No return value, called to print to the console \emph{detail} information about the \code{firm} object including:
\itemize{
  \item in the first paragraph:
  \itemize{
   \item legal form (if any),
   \item revenues (if known),
   \item capitalisation (if known).
  }
  \item in the second paragraph, the names of the board members/managers;
  \item in the third paragraph, a data frame with two columns:
  \itemize{
   \item First, the names of the owners
   \item The, their respective share of the firm's capital (normalised to 1)
  }

}
}
\description{
Print method for the S4 class representing a firm (legal person)
}
\author{
\enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
}
