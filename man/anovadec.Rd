\name{anovadec}
\alias{anovadec}
\title{Computation of several anovas on the output of a PCA}
\description{
A function to compute sum of squares decomposition on principal
components by using the aov function
}
\usage{
anovadec(Y, plan, ord.inter, nbcomp = 2)
}
\arguments{
  \item{Y}{data.frame of model output, usually the x component of a
    PCA object}
  \item{plan}{data.frame of input design}
  \item{ord.inter}{ANOVA formula like \code{"A+B+c+A:B"}
    OR an integer giving the maximum interaction order (1 for main effects)}
  \item{nbcomp}{number of principal components to be considered (e.g 3) }
}
\value{The anovadec function returns a two-component list:
  \describe{
  \item{aov}{list of AOV objects}
  \item{PC}{prediction of output}}
 }
\note{This is essentially an internal function for the multisensi package}
\author{M. LAMBONI}
\seealso{aov}
