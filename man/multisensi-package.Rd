\name{multisensi-package}
\alias{multisensi-package}
%\alias{multisensi}
\docType{package}
\title{Multivariate sensitivity Analysis}
\description{Sensitivity Analysis (SA) for models with multivariate output}
\details{
  This package generalises sensitivity analysis to simulation models with multivariate output. It makes it easy to run a series of independent sensitivity analyses on a set of output variables and to plot the results. Alternatively, it allows to apply sensitivity analyses to the variables resulting from the application of a multivariate method (such as PCA or splines or polynomial regression) to the output data (Lamboni et al., 2009).

The function \code{\link{multisensi}} integrates all the different possible methods implemented in the package. Besides, the user may consider the functions which have existed since the first version of the package:

i) \code{\link{gsi}} function for the Generalised Sensitivity Analysis  (Lamboni et al., 2011, Xiao and Li, 2016) based on inertia decomposition. This method synthesizes the
information that is spread between the time outputs or between the
principal components and produces a unique sensitivity index for each
factor.

ii) \code{\link{gsi}} function for the componentwise sensitivity analysis obtained by
computing sensitivity indices on principal components (Campbell et al., 2006)

iii) \code{\link{dynsi}} function for the dynamic sensitivity analysis obtained by
computing sensitivity indices on each output variable.

In the first version of \pkg{multisensi}, sensitivity indices were based on using a factorial design and a classical ANOVA decomposition. It is now possible to use other methods for the design and for the sensitivity analysis.

Simulation model management

The \pkg{multisensi} package works on simulation models coded either in \R or
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


\references{
Lamboni, M., Makowski, D., Monod, H., 2009. Multivariate global sensitivity analysis for dynamic crop models. Field Crops Research, volume 113, pp. 312-320.

Lamboni, M., Makowski, D., Monod, H., 2011. Multivariate sensitivity analysis to measure global contribution of input factors in dynamic models. Reliability Engineering & System Safety, volume 96, pp. 450-459.

Xiao, H., Li, L., 2016. Discussion of paper by M. Lamboni, H. Monod, D. Makowski Multivariate sensitivity analysis to measure global contribution of input factors in dynamic models, Reliab. Eng. Syst. Saf. 96 (2011) 450-459. Reliability Engineering & System Safety, volume 147, pp. 194-195.

Saltelli, A., Chan, K., Scott, E.M. eds, 2000. \emph{Sensitivity
Analysis} Wiley, New York.  
}

\author{C. Bidot, % \email{caroline.bidot@jouy.inra.fr}
M. Lamboni, %\email{clamboni@yahoo.fr}
H. Monod} %\email{herve.monod@jouy.inra.fr}

