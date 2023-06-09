% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02.FOandFM.R
\name{FO.norm}
\alias{FO.norm}
\title{Function to create a naive-valued firm-owner (FO) matrix}
\usage{
FO.norm(..., id_as_firm_name = NULL, Matrix = NULL)
}
\arguments{
\item{...}{Either multiple objects of class \code{firm} or a list of such objects}

\item{id_as_firm_name}{Whether to use the ticker as the firm's name. Defaults to \code{TRUE} if all firms' id is neither \code{NULL} nor \code{NA}.}

\item{Matrix}{Whether to use the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}. Defaults to \code{TRUE} when there are more than 10,000 combinations and the package is installed.}
}
\value{
A matrix object of class \code{financial_matrix} (possibly using the \href{https://cran.r-project.org/package=Matrix}{\code{Matrix} package}) in which:
\describe{
 \item{the rows}{Represent firms;}
 \item{the columns}{Represent owners (physical and legal persons).}
}
}
\description{
The values represent the share of firm \eqn{i}'s capital owned by \eqn{j}.
}
\examples{

# Create the normalised FO matrix of Berkshire Hathaway's holdings
\donttest{
data('firms_BKB')
FO <- FO.norm(firms_BKB)
}

}
\seealso{
\link{FM} \link{FO.binary} \link{FO.naive}
}
\author{
\enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
}
