\name{graph.bar}
\alias{graph.bar}
\title{Sensitivity index bar plot}
\description{
 A function that plots sensitivity indices by a bar graph
}
\usage{
graph.bar(x, col = 1, nb.plot = 15, xmax = NULL,
          beside = TRUE, ...)
}
\arguments{
  \item{x}{an object of class gsi or dynsi}
  \item{col}{the column number of GSI to represent in the bar graph}
  \item{nb.plot}{number of input factors to be considered}
  \item{xmax}{a user-defined maximal \eqn{x} value (\eqn{x\leq 1}) in
    all the bar graphs that show sensitivity indices; or NULL if the user
    wants to keep default values}
  \item{beside}{if TRUE, the main and total sensitivity indices are
    represented by two bars; if FALSE, they are represented by the same bar}
  \item{...}{graphical parameters}
}
\author{M. LAMBONI}
