\name{yapprox}
\alias{yapprox}
\title{Prediction based on PCA and anovas}
\description{
A function that predicts the model output after PCA and aov analyses
}
\usage{
yapprox(ACP, nbcomp = 2, aov.obj)
}
\arguments{
  \item{ACP}{prcomp object}
  \item{nbcomp}{number of PCs}
  \item{aov.obj}{aov object}
}
\value{A list with components
  \describe{
  \item{Y}{model output predictions}
  \item{trace}{model output inertia}
  }
 }
\note{This is essentially an internal function for the multisensi package}
\author{M. LAMBONI}
