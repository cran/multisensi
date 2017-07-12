\name{analysis.sensitivity}
\alias{analysis.sensitivity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Runs a series of sensitivity analyses by a function from the \pkg{sensitivity} package
}
\description{
  The \code{analysis.sensitivity} function runs a series of sensitivity analyses on the columns of a data.frame, using a method implemented in the \pkg{sensitivity} package.
}
\usage{
analysis.sensitivity(Y, plan, nbcomp = 2, sigma.car = NULL,
                     analysis.args = list(keep.outputs = FALSE))
}
\arguments{
  \item{Y}{a data.frame of output variables or principal components.
  }
  \item{plan}{an object containing the design. It must be created by a function from the \pkg{sensitivity} package with argument \code{model=NULL}.
  }
  \item{nbcomp}{the number of \code{Y} variables to analyse (the first \code{nbcomp} variables of \code{Y} will be analysed).
  }
  \item{sigma.car}{NULL or sum of squares of Y. If not NULL, compute the Generalised Sensitivity Indices (saved in the last column of the data.frame mSI/tSI/iSI outputs.
  }
  \item{analysis.args}{a list of arguments. If it contains \code{keep.outputs=TRUE}, the outputs associated with the analysis of each variable are returned (see section \code{Value}).
  }
}
\details{
  The argument \code{plan} must be an object created by a method implemented in the \pkg{sensitivity} package. Thus it belongs to a class such as \code{morris} or \code{fast99}. The name of the class is stored in the element \code{call.info$fct} of the output returned by \code{analysis.sensitivity}.
}
\value{A list containing:
  \item{SI}{data.frame of sensitivity indices or other importance measures returned by the function from the \pkg{sensitivity} package used. Sometimes empty but kept for compatibility reasons.}
  \item{mSI}{data.frame of first-order sensitivity indices}
  \item{tSI}{data.frame of total sensitivity indices}
  \item{iSI}{data.frame of interaction sensitivity indices}
  \item{inertia}{empty (kept for compatibility reasons)}
  \item{indic.fact}{0-1 matrix to indicate the factors associated with each factorial effect}
  \item{Hpredict}{empty (kept for compatibility reasons)}
  \item{outputkept}{if \code{ analysis.args$keep.outputs=TRUE}, list of the outputs returned by the sensitivity analysis performed on each variable}
  \item{call.info}{list with first element \code{analysis="sensitivity"} and second element \code{fct} storing the class name of the argument \code{plan}}
}
%\author{C. Bidot, H. Monod}
\examples{
# Test case : the Winter Wheat Dynamic Models (WWDM)
library(sensitivity) # to use fast99
# input factors design
data(biomasseX)
# input climate variable
data(Climat)

# example of the sensitivity:fast99 function
# design
newplan <- fast99(model = NULL, factors = names(biomasseX), n = 100, 
            q = "qunif", q.arg = list(list(min = 0.9, max = 2.8),
                         list(min = 0.9, max = 0.99),
                         list(min = 0.6, max = 0.8),
                         list(min = 3, max = 12),
                         list(min = 0.0035, max = 0.01),
                         list(min = 0.0011, max = 0.0025),
                         list(min = 700, max = 1100)))
# simulations
wwdm.Y <- simulmodel(model=biomasse, plan=newplan$X, climdata=Climat)
# analysis
res <- analysis.sensitivity(data.frame(wwdm.Y), plan=newplan, nbcomp=4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{sensitivity analysis}
\keyword{sensitivity package}% __ONLY ONE__ keyword per lin
