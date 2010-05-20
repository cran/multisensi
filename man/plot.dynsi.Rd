\name{plot.dynsi}
\alias{plot.dynsi}
\title{Plot method for dynamic sensitivity results}
\description{
  Plot method for dynamic sensitivity results of class dynsi
}
\usage{
\method{plot}{dynsi}(x, text.tuning = NULL, ...)
}
\arguments{
  \item{x}{a dynsi object}
  \item{text.tuning}{NULL or a small integer to improve the position of
    input factor labels}
  \item{...}{graphical parameters}
}
\details{
For labels that would be partly positioned outside the plot frame, the argument
"text.tuning" may allow to get a better positioning. If it is equal to
\eqn{n}, say, these labels are moved by \eqn{n} positions inside the
frame, where 1 position corresponds to 1 output variable on the x-axis.
}
\seealso{\code{\link{dynsi}}}
\author{M. LAMBONI}
