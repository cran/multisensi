\name{acp}
\alias{ACP}
\title{Principal Component Analysis (PCA)}
\description{
  Principal Component Analysis (PCA) for the generalized sensitivity analysis
}
\usage{
ACP(simuls, normalized = TRUE)
}
\arguments{
  \item{simuls}{data frame, typically a multivariate model output}
  \item{normalized}{If TRUE, a normalized PCA is performed}
}
\value{an object of class prcomp; see the \code{prcomp} help for further
  details}
\note{This is essentially an internal function for the multisensi package}
\author{M. LAMBONI}
\seealso{prcomp}
