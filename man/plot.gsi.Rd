\name{plot.gsi}
\alias{plot.gsi}
\title{Plot method for generalised sensitivity analysis}
\description{
  Plot method for generalised sensitivity analysis of class gsi
}
\usage{
\method{plot}{gsi}(x, nb.plot = 10, nb.comp = 3, graph = 1:3, xmax=NULL,
                   beside=TRUE, cor.plot=FALSE, xtick=TRUE, type="l",...)
}
\arguments{
  \item{x}{a gsi object.}
  \item{nb.plot}{number of input factors to be considered.}
  \item{nb.comp}{number of Principal Components to be plotted.}
  \item{graph}{figures number: 1 or 2 or 3. 1 is for plotting the PCs and their sensitivity indices, 2 is for plotting the Generalised Sensitivity Indice, 3 is for plotting the Rsquare.}
  \item{xmax}{a user-defined maximal \eqn{x} value (\eqn{x\leq 1}) in
    all the bar graphs that show sensitivity indices; or NULL if the user
    wants to keep default values.}
  \item{beside}{if TRUE, the main and total sensitivity indices are
    represented by two bars; if FALSE, they are represented by the same bar.}
  \item{cor.plot}{if TRUE a correlation graph is made to represent the PCs ; if FALSE (default) a functionnal boxplot of the PCs is plotted.}
  \item{xtick}{if TRUE, put column names of outputs (Y) as ticks for the x axis.}
  \item{type}{what type of plot should be drawn for correlation graph ("l" for lines).}
  \item{...}{graphical parameters.}
}
\seealso{\code{\link{gsi}}, \code{\link{multisensi}}, \code{\link{graph.bar}}, \code{\link{graph.pc}}}
%\author{C. Bidot, M. Lamboni, H. Monod}
