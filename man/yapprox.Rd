\name{yapprox}
\alias{yapprox}
\title{Prediction based on PCA and anovas (NOT ONLY)}
\description{
A function that predicts the model output after PCA and aov analyses
}
\usage{
yapprox(multivar.obj, nbcomp = 2, aov.obj)
}
\arguments{
  \item{multivar.obj}{output of the multivar function}
  \item{nbcomp}{number of columns }
  \item{aov.obj}{aov object}
}
\value{model output predictions}
  
\note{This is essentially an internal function for the \pkg{multisensi} package}
%\author{C. Bidot, M. Lamboni}
\keyword{internal function}
