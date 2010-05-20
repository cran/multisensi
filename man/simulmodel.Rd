\name{simulmodel}
\alias{simulmodel}
\title{Model simulation}
\description{
 Function that simulates the model outputs
}
\usage{
simulmodel(model, plan, nomFic = NULL, verbose = FALSE, ...)
}
\arguments{
  \item{model}{name of R-function}
  \item{plan}{data frame of input design}
  \item{nomFic}{name of file that contains the model function}
  \item{verbose}{verbose}
  \item{\dots}{... }{possible fixed parameters of the R-function}
}
\details{
The model function must be a R-functions. Models defined as functions
will be called once with an expression of the form \code{y <- f(X)} where \code{X} is
a vector containing a combination of levels of the input factors, and \code{y}
is the output vector of length \eqn{q}, where \eqn{q} is the number of output
variables}
\value{data frame of model outputs}
\note{This is essentially an internal function for the multisensi package}
\author{M. LAMBONI}
