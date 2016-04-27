\name{basis.ACP}
\alias{basis.ACP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  A function to decompose multivariate data by principal components analysis (PCA)
}
\description{
  The \code{basis.ACP} function decomposes a multivariate data set according to principal components analysis.
}
\usage{
basis.ACP(simuls, basis.args = list())
}
\arguments{
  \item{simuls}{a data.frame of size \code{N x T}, typically a set of \code{N} simulation outputs  of length \code{T}.
  }
  \item{basis.args}{an empty list of arguments for the PCA decomposition. 
  }
}
\details{
  This function uses \code{\link[stats]{prcomp}}.
}
\value{
\item{H}{a data.frame of size \code{N x T}, containing the coefficients of the PCA decomposition. It is equal to the \code{x} output of function \code{\link[stats]{prcomp}}.}
\item{L}{a matrix of size \code{T x T}. It contains the eigenvectors of the PCA decomposition.}
\item{call.info}{list with the element \code{reduction="pca"}}
 }

\seealso{
\code{\link[stats]{prcomp}}
}
\examples{
data(biomasseY)
res <- basis.ACP(biomasseY)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{principal components analysis}
\keyword{dimension reduction}
