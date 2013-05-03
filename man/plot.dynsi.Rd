\name{plot.dynsi}
\alias{plot.dynsi}
\title{Plot method for dynamic sensitivity results}
\description{
  Plot method for dynamic sensitivity results of class dynsi
}
\usage{
\method{plot}{dynsi}(x, text.tuning = NULL, shade=FALSE, color=NULL, ...)
}
\arguments{
  \item{x}{a dynsi object}
  \item{text.tuning}{NULL or a small integer to improve the position of
    input factor labels}
  \item{shade}{if TRUE, put different shadings to enhance the different
    factorial effects in the plot (long)}
  \item{color}{a palette of colors to enhance the different
    factorial effects in the plot (for example \code{color=heat.colors})}
  \item{...}{graphical parameters}
}
\details{
For labels that would be partly positioned outside the plot frame, the argument
"text.tuning" may allow to get a better positioning. If it is equal to
\eqn{n}, say, these labels are moved by \eqn{n} positions inside the
frame, where 1 position corresponds to 1 output variable on the x-axis.
}
\note{changed at version 1.0-6 (May 2013)}
\seealso{\code{\link{dynsi}}}
\author{M. LAMBONI, H. MONOD}
