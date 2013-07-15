\name{gsi}
\alias{gsi}
\title{Generalized Sensitivity Indices: GSI}
\description{The gsi function implements the calculation of Generalised Sensitivity
  Indices. This method allows to compute a synthetic Sensitivity Index for
  the dynamic or multivariate models by using factorial designs and the
  MANOVA decomposition of inertia. It computes also the Sensitivity
  Indices on principal components}
\usage{
gsi(formula, model, factors, inertia = 0.95, normalized = TRUE,
    cumul = FALSE, simulonly = FALSE, Name.File = NULL, ...)}
\arguments{
  \item{formula}{ANOVA formula like \code{"A+B+C+A:B"} OR an integer equal to the
    maximum interaction order in the sensitivity model}
  \item{model}{output data.frame OR the name of the R-function which
    calculates the model output. The only argument of this function
    must be a vector containing the input factors values}
  \item{factors}{input data.frame (the design) if model is a data.frame
    OR a list of factors levels such as :
    \code{factor.example <- list(A=c(0,1),B=c(0,1,4))}}
  \item{inertia}{cumulated proportion of inertia (a scalar \eqn{< 1}) to be
    explained by the selected Principal components OR number of PCs to
    be used (e.g 3)}
  \item{normalized}{logical value. TRUE (default) computes a normalized
    Principal Component analysis.}
  \item{cumul}{logical value. If TRUE the PCA will be done on the cumulative outputs}
  \item{simulonly}{logical value.  If TRUE the program stops after
    calculating the design and the model outputs}
  \item{Name.File}{optional name of a R script file containing the
    R-function that calculates the simulation model. e.g  \code{"exc.ssc"}}
  \item{...}{possible fixed parameters of the model function}
}
\details{
  If factors is a list of factors, the gsi function generates a complete
  factorial design. If it is a data.frame, gsi expects that each column is
  associated with an input factor.
}
\value{gsi returns a list of class "gsi", containing all the input arguments detailed before, plus
  the following components:
  \item{X}{a data.frame containing the experimental design (input samples)}
  \item{Y}{a data.frame containing the output matrix (response)}
  \item{H}{a data.frame containing the principal components}
  \item{L}{a data.frame whose columns contain the basis eigenvectors (the variable loadings)}
  \item{lambda}{the variances of the principal components}
  \item{inertia}{vector of inertia percentages per PCs and global criterion}
  \item{cor}{a data.frame of correlation between PCs and outputs}
  \item{SI}{a data.frame containing  the Sensitivity Indices (SI) on PCs
    and the Generalized SI (GSI)}
  \item{mSI}{a data.frame of first order SI on PCs and first order GSI}
  \item{tSI}{a data.frame containing the total SI on PCs and the total GSI}
  \item{iSI}{a data.frame of interaction SI on PCs and interaction GSI}
  \item{pred}{a data.frame containing the output predicted by the
    metamodel arising from the PCA and anova decompositions}
  \item{residuals}{a data.frame containing the residuals between actual
    and predicted outputs}
  \item{Rsquare}{vector of dynamic coefficient of determination}
  \item{Att}{0-1 matrix of association between input factors and
    factorial terms in the anovas}
  \item{normalized}{logical value, see the arguments}
  \item{cumul}{logical value, see the arguments}
  ...
}
\references{
  M. Lamboni, D. Makowski and H. Monod, 2009. Multivariate
  global sensitivity analysis for dynamic
  crop models. Field Crops Research, volume 113.  pp. 312-320

  M. Lamboni, D. Makowski and H. Monod, 2009. Multivariate sensitivity analysis to measure global
  contribution of input factors in dynamic models. Submitted
  to Reliability Engineering  and System Safety.
}
\author{ M. Lamboni}
\seealso{dynsi}
\examples{
##---- Should be DIRECTLY executable !! ----
# Test case : the Winter Wheat Dynamic Models (WWDM)
#  input factors design
 data(plan)
# input climate variable
 data(Climat)
# output variables (precalculated to speed up the example)
 data(biomasseY)
#
 GSI <- gsi(2, biomasseY, plan, inertia=3, normalized=TRUE, cumul=FALSE,
            climdata=Climat)
 summary(GSI)
 print(GSI)
 plot(x=GSI, beside=FALSE)
 #plot(GSI, nb.plot=4)         # the 'nb.plot' most influent factors
                               # are represented in the plots
 #plot(GSI,nb.comp=2, xmax=1)  # nb.comp = number of principal components
 #plot(GSI,nb.comp=3, graph=1) # graph=1 for first figure; 2 for 2nd one
                               # and 3 for 3rd one; or 1:3 etc.
 #graph.bar(GSI,col=1, beside=F)  # sensitivity bar plot on the first PC
 #graph.bar(GSI,col=2, xmax=1)    #
  }
