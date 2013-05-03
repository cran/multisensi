\name{acp}
\alias{ACP}
\title{Principal Component Analysis (PCA)}
\description{
  Principal Component Analysis (PCA) for the generalized sensitivity analysis
}
\usage{
ACP(simuls, inertia=0.95, normalized = TRUE)
}
\arguments{
  \item{simuls}{data frame, typically a multivariate model output}
  \item{inertia}{inertia proportion account by Principal Components <1 (0.95 default ) OR number of PCs to be used (E.g 3)}
  \item{normalized}{If TRUE, a normalized PCA is performed}
}
\value{
  \item{x}{output x of prcomp function ; see the \code{prcomp} help for further details}
  \item{rotation}{the matrix of variable loadings}
  \item{sdev}{the standard deviations of the Principal Components}
  \item{nbcomp}{the number of Principal Components}
}
\note{This is essentially an internal function for the multisensi package}
\author{M. LAMBONI}
\seealso{prcomp}
