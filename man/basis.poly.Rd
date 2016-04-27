\name{basis.poly}
\alias{basis.poly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  A function to decompose multivariate data on a polynomial basis
}
\description{
  The \code{basis.poly} function decomposes a multivariate data set on a polynomial basis.
}
\usage{
basis.poly(simuls, basis.args = list(degree = 3))
}
\arguments{
  \item{simuls}{a data.frame of size \code{N x T}, typically a set of \code{N} simulation outputs of length \code{T}.
  }
  \item{basis.args}{a list of arguments for the polynomial decomposition. The \code{degree} argument is the maximum degree of the polynomial basis. For the optional \code{x.coord} argument, see the Details section.
  }
}
\details{
  This function uses \code{\link[stats]{poly}}. The optional \code{x.coord} element of the list in \code{basis.args} can be used to specify the support of the polynomial decomposition, if different from \code{1:T}. It must be a vector of length \code{T}.
}
\value{
\item{H}{a data.frame of size \code{N x (d+1)}, where \code{d} is the degree of the polynomial decomposition. It contains the coefficients of the decomposition for each row of the \code{simuls} data.frame.}
\item{L}{a matrix of size \code{T x (d+1)}. It contains the vectors of the polynomial basis.}
\item{call.info}{list with the element \code{reduction="polynomial"}}
 }
\seealso{
\code{\link[stats]{poly}}
}
\examples{
data(biomasseY)

res <- basis.poly(biomasseY,basis.args=list(degree=3))

}
\keyword{polynomial basis}
\keyword{dimension reduction}
