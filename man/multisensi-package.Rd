\name{multisensi-package}
\alias{multisensi-package}
\alias{multisensi}
\docType{package}
\title{Multivariate sensitivity Analysis}
\description{Sensitivity Analysis (SA) for models with multivariate output}
\details{

This package contains three methods for performing sensitivity analysis
on simulation models with multivariate output:

i) \code{\link{gsi}} function for the Generalised Sensitivity Analysis  (Lamboni et
al., 2009) based on inertia decomposition. This method synthesizes the
information that is spread between the time outputs or between the
principal components and produces a unique sensitivity index for each
factor.

ii) \code{\link{gsi}} function for the componentwise sensitivity analysis obtained by
computing sensitivity indices on principal components (Campbell et al., 2006)

iii) \code{\link{dynsi}} function for the dynamic sensitivity analysis obtained by
computing sensitivity indices on each output variable.

For all three methods, sensitivity indices are calculated presently by
using a factorial design and a classical ANOVA decomposition.

Simulation model management

The multisensi package works on simulation models coded either in \R or
using an external language (typically as an executable file). Models
coded in \R must be either functions or objects that have a predict
method, such as lm objects. Models defined as functions will be called
once with an expression of the form \code{y <- f(X)} where \code{X} is a vector
containing a combination of levels of the input factors, and \code{y} is the
output vector of length \eqn{q}, where \eqn{q} is the number of output variables. If
the model is external to \R, for instance a computational code, it must
be analyzed with the decoupled approach: the methods require an input
data frame (\code{X}) containing all the combinations of the input levels and
the outputs data frame (\code{Y}) containing the response of the model
corresponding to these combinations.  The size of \code{X} is \eqn{n*p} and the size
of \code{Y} is \eqn{n*q} where \eqn{p} is the number of the input factor, \eqn{q} is the number
of the model outputs and \eqn{n} is the number of all the combinations of the
input levels.  This approach can also be used on \R models that do not
fit the required specifications.  

}


\references{Lamboni, M., Makowski, D., Monod, H., 2009. Multivariate
  global sensitivity analysis for dynamic crop models. Field Crops
  Research, volume 113.  pp. 312-320

Lamboni, M., Makowski, D., Monod, H., 2009. Multivariate sensitivity
    analysis to measure global contribution of input factors in dynamic
    models (submitted in october 2009 to Reliability
    Engineering \& System Safety

Saltelli, A., Chan, K., Scott, E.M. eds, 2000. \emph{Sensitivity
Analysis} Wiley, New York.  }
\author{Matieyendou Lamboni <clamboni@yahoo.fr>, Herv\'{e} Monod <herve.monod@jouy.inra.fr>, Caroline Bidot <caroline.bidot@jouy.inra.fr>}

