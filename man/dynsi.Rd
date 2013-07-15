\name{dynsi}
\alias{dynsi}
\title{Dynamic Sensitivity Indices: DSI}
\description{dynsi implements the Dynamic Sensitivity Indices. This method
  allows to compute classical Sensitivity Indices on each output
  variable of a dynamic or multivariate model by using the ANOVA
  decomposition}
\usage{
dynsi(formula, model, factors, cumul = FALSE, simulonly=FALSE,
      nb.outp = NULL, Name.File=NULL, ...)
}
\arguments{
  \item{formula}{ANOVA formula like \code{"A+B+c+A:B"} OR an integer equal to the
    maximum interaction order in the sensitivity model}
  \item{model}{output data.frame OR the name of the R-function which
    calculates the model output. The only argument of this function
    must be a vector containing the input factors values}
  \item{factors}{input data.frame (the design) if model is a data.frame
    OR a list of factors levels such as
    \code{factor.example <- list(A=c(0,1),B=c(0,1,4))}}
  \item{cumul}{logical value. If TRUE the sensitivity analysis will be
    done on the cumalative outputs}
  \item{simulonly}{logical value. If TRUE the program stops after
    calculating the design and the model outputs}
  \item{nb.outp}{The first nb.outp number of model outputs to be
    considered. If NULL all the outputs are considered}
  \item{Name.File}{optional name of a R script file containing the
    R-function that calculates the simulation model. e.g  \code{"exc.ssc"}}
  \item{...}{possible fixed parameters of the model function}
}
\details{
 If factors is a list of factors, the dynsi function generates a complete
 factorial design. If it is a data.frame, dynsi expects that each column is
 associated with an input factor.
}
\value{dynsi returns a list of class "dynsi", containing all the input arguments detailed before, plus
  the following components:
  \item{X}{a data.frame containing the experimental design (input samples)}
  \item{Y}{a data.frame containing the output matrix (response)}
  \item{SI}{a data.frame containing  the Sensitivity Indices (SI) on each
    output variable of the model}
  \item{mSI}{a data.frame of principal SI on each output variable}
  \item{tSI}{a data.frame containing the total SI on each output variable}
  \item{iSI}{a data.frame of interaction SI on each output variable}
  ...
}
\references{
  M. Lamboni, D. Makowski and H. Monod, 2009. Multivariate
  global sensitivity analysis for dynamic crop models.
  Field Crops Research, 113, 312-320.

  A. Saltelli, K. Chan and E. M. Scott eds, 2000. \emph{Sensitivity
  Analysis}. Wiley, New York.
}
\author{Matieyendou LAMBONI}
\seealso{gsi}
\examples{
##---- Should be DIRECTLY executable !! ----
# Test case : the Winter Wheat Dynamic Models (WWDM)
# input factors design,
 data(plan)
# input Climate variables
 data(Climat)
# output variables (precalculated to speed up the example)
 data(biomasseY)
#
 DYNSI <- dynsi(2, biomasseY, plan)
 summary(DYNSI)
 print(DYNSI)
 plot(DYNSI, color=heat.colors)
 #graph.bar(DYNSI,col=1, beside=F)  # sensitivity bar plot
                                    # for the first output (col=1)
 #graph.bar(DYNSI,col=2, xmax=1)    #
  }
