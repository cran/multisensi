\name{planfact}
\alias{planfact}
\title{Complete factorial design in lexical order}
\description{
  Function that generates a complete factorial design in lexical order
}
\usage{
planfact(nb.niv, make.factor = TRUE)
}
\arguments{
  \item{nb.niv}{vector containing the number of each input levels}
  \item{make.factor}{logical value. If TRUE the columns of the output
    are of class factor}
}
\value{
  \item{plan}{data frame of the complete factorial design}
}
\note{This is essentially an internal function for the \pkg{multisensi} package}
\author{M. Lamboni}
\keyword{internal function}
