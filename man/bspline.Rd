\name{bspline}
\alias{bspline}
\title{
  function to evaluate B-spline basis functions
}
\description{The \code{bspline} function evaluates ith B-spline basis function of order m at the values in x, given knot locations in k
}
\usage{
bspline(x = seq(0, 1, len = 101), k = knots, i = 1, m = 2)
}
\details{ B-splines are defined by recursion :
\eqn{b_{i,0}(x) = 1}{b_{i,0}(x) = 1} if \eqn{k_j \leq x < k_{j+1}}{k_j <= x < k_{j+1}} ; 0 else. 
 \deqn{b_{i,m}(x) = \frac{x-k_i}{k_{i+m}-k_i} b_{i,m-1}(x) + \frac{k_{i+m+1}-x}{k_{i+m+1}-k_{i+1}} b_{i+1,m-1}(x)}{b_{i,m}(x) = (x-k_i)/(k_{i+m}-k_i) b_{i,m-1}(x) + (k_{i+m+1}-x)(k_{i+m+1}-k_{i+1}) b_{i+1,m-1}(x)}

}
\arguments{
  \item{x}{
vector or scalar, coordinate where to calculate the B-spline functions
}
  \item{k}{
vector of knot locations
}
  \item{i}{integer; from 0 to length(knots)+1-m
}
  \item{m}{integer, degree of the B-Splines
}
}
\value{values in x of the ith B-spline basis function of order m
}
\note{This is essentially an internal function for the \pkg{multisensi} package}

\references{Wood Simon, 2006. \emph{Generalized Additive Models: An Introduction with R} Chapman and Hall/CRC.
}
\keyword{B-spline}
\keyword{internal function}
