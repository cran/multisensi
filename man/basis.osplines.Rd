\name{basis.osplines}
\alias{basis.osplines}
\title{
  A function to decompose multivariate data on an orthogonal B-spline basis (O-spline)
}
\description{The \code{basis.osplines} function decomposes a multivariate data set on an orthogonalised B-spline (or O-spline) basis defined by its knots and mdegree parameters.
}
\usage{
basis.osplines(simuls, basis.args = list(knots = 5, mdegree = 3))
}
\arguments{
  \item{simuls}{a data.frame of size \code{N x T}, typically a set of \code{N} simulation outputs of length \code{T}.
  }
  \item{basis.args}{a list of arguments for the O-spline decomposition. The \code{knots} argument is the number of knots or the vector of knot positions. The \code{mdegree} argument is the polynomial degree. For the optional \code{x.coord} argument, see the Details section.
  }
}
\details{
The optional \code{x.coord} element of the list in \code{basis.args} can be used to specify the support of the O-spline decomposition, if different from \code{1:T}. It must be a vector of length \code{T}.
}
\value{
  \item{H}{a data.frame of size \code{N x d}, where \code{d} is the dimension of the O-spline decomposition. It contains the coefficients of the decomposition for each row of the \code{simuls} data.frame.}
  \item{L}{a matrix of size \code{T x d}. It contains the vectors of the O-spline basis.}
\item{call.info}{list with the element \code{reduction="o-splines"}}
}
\seealso{\code{\link{bspline}},
\code{\link{sesBsplinesORTHONORM}}
}
\examples{
data(biomasseY)

res <- basis.osplines(biomasseY,basis.args=list(knots=7,mdegree=3))

}
\keyword{orthogonalized B-spline}
\keyword{dimension reduction}% __ONLY ONE__ keyword per line
