\name{analysis.anoasg}
\alias{analysis.anoasg}
\title{
Runs a series of analyses of variance}
\description{The \code{analysis.anoasg} function runs a series of analyses of variance on the columns of a data.frame, by using the \code{\link[stats]{aov}} function.
}
\usage{
analysis.anoasg(Y, plan, nbcomp = 2, sigma.car = NULL,
                analysis.args = list(formula = 2,
                                     keep.outputs = FALSE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{a data.frame of output variables or principal components.
  }
  \item{plan}{a data.frame containing the design.
  }
  \item{nbcomp}{the number of \code{Y} variables to analyse (the first \code{nbcomp} variables of \code{Y} will be analysed).
  }
  \item{sigma.car}{NULL or sum of squares of Y. If not NULL, compute the Generalised Sensitivity Indices (saved in the last column of the data.frame mSI/tSI/iSI outputs.
  }
  \item{analysis.args}{a list of arguments. The \code{formula} component is for ANOVA formula like \code{"A+B+c+A:B"} OR an integer giving the maximum interaction order (1 for main effects). If it contains \code{keep.outputs=TRUE}, the outputs associated with the analysis of each variable are returned (see section \code{Value}).
  }
}
\value{A list containing:
  \item{SI}{data.frame of sensitivity indices}
  \item{mSI}{data.frame of first-order sensitivity indices}
  \item{tSI}{data.frame of total sensitivity indices}
  \item{iSI}{data.frame of interaction sensitivity indices}
  \item{inertia}{vector of Inertia explained by the variables}
  \item{indic.fact}{0-1 matrix to indicate the factors associated with each factorial effect}
  \item{Hpredict}{prediction of outputs}
  \item{outputkept}{if \code{ analysis.args$keep.outputs=TRUE}, list of the outputs returned by the sensitivity analysis performed on each variable}
  \item{call.info}{list with first element \code{analysis="anova"}}
}


\seealso{\code{\link[stats]{aov}}}
\examples{
# Test case : the Winter Wheat Dynamic Models (WWDM)
#  input factors design
 data(biomasseX)
# output variables (precalculated to speed up the example)
 data(biomasseY)

 res <- analysis.anoasg(biomasseY, biomasseX, 
                        nbcomp = 2, sigma.car = NULL,
                        analysis.args = list(formula = 2,
                                             keep.outputs = FALSE))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{sensitivity analysis}
