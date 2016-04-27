\name{sesBsplinesORTHONORM}
\alias{sesBsplinesORTHONORM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
orthogonalized B-splines basis functions
}
\description{
The \code{sesBsplinesORTHONORM} evaluates O-Splines basis functions at some points.
}
\usage{
sesBsplinesORTHONORM(x = seq(0, 1, len = 101), knots = 5, m = 2)
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
  \item{osplines}{matrix, values in x of all O-spline basis functions of order m}
  \item{knots}{vector of knots locations}
  \item{projecteur}{inverse matrix of \code{osplines}}
}
\note{This is essentially an internal function for the \pkg{multisensi} package}

\seealso{\code{\link{bspline}}, \code{\link{basis.osplines}}}
\author{C. Bidot, J. Baudet}
\keyword{internal function}
\keyword{orthogonalized B-spline}
