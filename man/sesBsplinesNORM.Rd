\name{sesBsplinesNORM}
\alias{sesBsplinesNORM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
normalized B-splines basis functions
}
\description{
The \code{sesBsplinesNORM} evaluates B-Splines basis functions at some points.

}
\usage{
sesBsplinesNORM(x = seq(0, 1, len = 101), knots = 5, m = 2)
}
\arguments{
  \item{x}{
vector, coordinates where to calculate the B-spline functions
}
  \item{knots}{
number of knots or vector of knots locations
}
  \item{m}{
integer, degree of the B-Splines
}
}
\value{
  \item{x}{as input}
  \item{bsplines}{matrix, values in x of all B-spline basis functions of order m}
  \item{knots}{vector of knots locations}
  \item{projecteur}{inverse matrix of \code{bsplines}}
}
\note{This is essentially an internal function for the \pkg{multisensi} package}

\seealso{\code{\link{bspline}}, \code{\link{basis.bsplines}}}
\author{C. Bidot, J. Baudet}
\keyword{internal function}
\keyword{B-spline}
