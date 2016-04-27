\name{planfact.as}
\alias{planfact.as}
\title{Complete factorial design}
\description{
 Computation of a complete factorial design for model input factors.
}
\usage{
planfact.as(input)
}
\arguments{
  \item{input}{list of factor levels}
}
\value{
    \item{comp2}{complete factorial design of model input}
}
\note{This is essentially an internal function for the \pkg{multisensi}
  package. It is almost equivalent to the function \code{\link[base]{expand.grid}}.}
\author{M. Lamboni}
\keyword{internal function}
