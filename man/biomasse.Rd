\name{biomasse}
\alias{biomasse}
\title{The Winter Wheat Dynamic Model}
\description{
 The Winter Wheat Dynamic Model, a toy model to illustrate the main
 multisensi methods
}
\usage{
biomasse(input, climdata, annee = 3)
}
\arguments{
  \item{input}{vector of input values.}
  \item{annee}{year.}
  \item{climdata}{a meteorological data.frame specific to biomasse.}
}
\details{
The Winter Wheat Dry Matter model (WWDM) is a dynamic crop model
running at a daily time step (Makowski et al, 2004). It has two state
variables, the above-ground winter wheat dry matter \eqn{U(t)}, in \eqn{g/m^2}
and the leaf area index LAI(\eqn{t}) with \eqn{t} the day number from sowing
(\eqn{t=1}) to harvest (\eqn{t=223}). In the \pkg{multisensi} package implementation, the
\code{biomasse} function simulates the output for only one parameter set (the
first row of \code{input} if it is a matrix or a data.frame).
}
\value{
 a vector of daily dry matter increase of the Winter Wheat biomass, over
 223 days
}
\references{
Makowski, D., Jeuffroy, M.-H., Gu\'{e}rif, M., 2004 Bayesian methods for
updating crop model predictions, applications for predicting biomass and
grain protein content. In: Bayesian Statistics and Quality Modelling in
the Agro-Food Production Chain (van Boeakel et al. eds), pp. 57-68.
Kluwer, Dordrecht

Monod, H., Naud, C., Makowski, D., 2006 Uncertainty and sensitivity
analysis for crop models. In: Working with Dynamic Crop Models (Wallach
D., Makowski D. and Jones J. eds), pp. 55-100. Elsevier, Amsterdam}
%\author{initially Makowski, D., 2004}
