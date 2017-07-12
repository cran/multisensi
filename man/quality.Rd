\name{quality}
\alias{quality}
\title{quality of any approximation}
\description{
 Function that computes the sensitivity quality after making some assumptions about the number of PCs and the number of interactions
}
\usage{
quality(echsimul, echsimul.app)
}
\arguments{
  \item{echsimul}{model outputs}
  \item{echsimul.app}{Predicted model output}
}
\value{A list with the following components: 
  \describe{
  \item{moy.biais}{mean of the residuals}
  \item{residuals}{biais}
  \item{coef.det}{R-square}
  }
}
\note{This is essentially an internal function for the \pkg{multisensi} package}
%\author{C. Bidot, M. Lamboni}
\keyword{internal function}
