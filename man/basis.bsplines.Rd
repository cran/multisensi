\name{basis.bsplines}
\alias{basis.bsplines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  A function to decompose multivariate data on a B-spline basis
}
\description{
  The \code{basis.bsplines} function decomposes a multivariate data set on a B-spline basis defined by its knots and mdegree parameters.
}
\usage{
basis.bsplines(simuls, basis.args = list(knots = 5, mdegree = 3))
}
\arguments{
  \item{simuls}{a data.frame of size \code{N x T}, typically a set of \code{N} simulation outputs of length \code{T}.
  }
  \item{basis.args}{a list of arguments for the B-spline decomposition. The \code{knots} argument is the number of knots or the vector of knot positions. The \code{mdegree} argument is the polynomial degree. For the optional \code{x.coord} argument, see the Details section.
  }
}
\details{
The optional \code{x.coord} element of the list in \code{basis.args} can
be used to specify the support of the B-spline decomposition, if
different from \code{1:T}. It must be a vector of length \code{T}.
}
\value{
\item{H}{a data.frame of size \code{N x d}, where \code{d} is the dimension of the B-spline decomposition. It contains the coefficients of the decomposition for each row of the \code{simuls} data.frame.}
\item{L}{a matrix of size \code{T x d}. It contains the vectors of the B-spline basis.}
\item{call.info}{list with the element \code{reduction="b-splines"}}
 }


\seealso{\code{\link{bspline}}, \code{\link{sesBsplinesNORM}}
}
\examples{
data(biomasseY)

res <- basis.bsplines(biomasseY,basis.args=list(knots=7,mdegree=3))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{B-spline}
\keyword{dimension reduction}% __ONLY ONE__ keyword per line
