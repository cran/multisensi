\name{quality}
\alias{quality}
\title{quality of any approximation}
\description{
 Function that computes the sensitivity quality after making some
 assumptions about the number of PCs and the number of interactions
}
\usage{
quality(echsimul, echsimul.app, normalise = TRUE)
}
\arguments{
  \item{echsimul}{model outputs}
  \item{echsimul.app}{Predicted model output}
  \item{normalise}{logical value}
}
\value{A list with the following components:
  \describe{
  \item{moy}{mean}
  \item{biais}{biais}
  \item{coef.det}{R-square}
  }
}
\note{This is essentially an internal function for the multisensi package}
\author{ M. LAMBONI}
