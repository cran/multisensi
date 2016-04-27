\name{basis.mine}
\alias{basis.mine}
\title{
  A function to decompose multivariate data on a user-defined basis
}
\description{
  The \code{basis.mine} function decomposes a multivariate data set on a user-defined basis.
}
\usage{
basis.mine(simuls, basis.args = list(
        baseL=1*outer(sort(0:(ncol(simuls)-1)\%\%5),0:4,"==") ) )
}
\arguments{
  \item{simuls}{a data.frame of size \code{N x T}, typically a set of \code{N} simulation outputs of length \code{T}.
  }
  \item{basis.args}{a list of arguments for the polynomial decomposition. The \code{baseL} argument is a matrix of size \code{T x d} containing the coordinates of the \code{d} basis vectors.
  }
}
\value{
\item{H}{a data.frame of size \code{N x d}, where \code{d} is the number of basis vectors. It contains the coefficients of the decomposition for each row of the \code{simuls} data.frame.}
\item{L}{a matrix of size \code{T x d}. It contains the vectors of the user-defined basis.}
\item{call.info}{list with the element \code{reduction="matrix"}}
 }

\details{ The default \code{basis.args} argument generates a projection on a moving-average basis. But if in the \code{\link{multisensi}} function this \code{basis.args} argument is not given for \code{reduction=basis.mine}, the execution will be stopped.
}

\examples{
data(biomasseY)
M <- 1*outer(sort(0:(ncol(biomasseY)-1)\%\%5),0:4,"==")
norm.M <- sqrt(colSums(M^2)) 
for (i in 1:ncol(M)){
    M[,i]=M[,i]/norm.M[i]
}

res <- basis.mine(biomasseY, basis.args=list(baseL=M))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{basis}
\keyword{dimension reduction}% __ONLY ONE__ keyword per line
