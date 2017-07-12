\name{multivar}
\alias{multivar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  A function to decompose the output data set and reduce its dimension
}
\description{The function \code{multivar} applies a multivariate method
  to decompose the output variables on a given basis.
}
\usage{
multivar(simuls, dimension = NULL, reduction, centered = TRUE, 
         scale = TRUE, basis.args = list())
}
\arguments{
  \item{simuls}{a data.frame of size \code{N x T}, typically a set of \code{N} simulation outputs of length \code{T}
  }
  \item{dimension}{the number of variables to analyse, specified by an integer (for example 3) or by the minimal proportion of inertia (for example 0.95) to keep in the output decomposition
  }
  \item{reduction}{a function to decompose the multivariate output on a basis of smaller dimension
  }
  \item{centered}{logical value. If TRUE the output variables are centred.}
  \item{scale}{logical value. If TRUE the output variables are normalized.}
  \item{basis.args}{
    a list of arguments for the function given in the \code{reduction} argument. See the function help for more precision.
  }
}
\value{A list containing:
  \item{H        }{a data.frame of size \code{N x d}, where \code{d} is the number of basis vectors. It contains the coefficients of the decomposition for each row of the \code{simuls} data.frame.
  }
\item{L        }{a matrix of size \code{T x d}. It contains the vectors of the user-defined basis.}
\item{sdev     }{standard deviations of the columns of \code{H} }
\item{nbcomp   }{number of components kept from the decomposition}
\item{SStot    }{total sums of squares of the simulations (after application of \code{centered} and \code{scale})}
\item{centering}{either 0 or the column averages of \code{simuls}}
\item{scaling  }{either 1 or \code{sdY}, depending on the \code{scale} argument}
\item{sdY      }{standard deviations of the columns of \code{simuls}}
\item{cor      }{correlation matrix (L*sdev), of size \code{T x nbcomp}}
\item{scale    }{kept in case the option scale has been changed in the function}
\item{importance}{cumulated percentage of SS_H (sdev^2) with respect to SStot}
\item{call.info}{list with the element \code{reduction} storing the name of the argument \code{reduction}}
}


\seealso{\code{\link{basis.ACP}}, \code{\link{basis.bsplines}}, \code{\link{basis.poly}}, \code{\link{basis.osplines}}
}
\examples{
data(biomasseY)

res <- multivar(biomasseY, dimension=0.95, reduction=basis.ACP)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate analysis}
\keyword{dimension reduction}% __ONLY ONE__ keyword per line
