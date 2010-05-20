\name{graph.pc}
\alias{graph.pc}
\title{Principal Components graph for gsi objects}
\description{
  A function that plots the Principal components (PCs) and the
  sensitivity index on each PC
}
\usage{
graph.pc(x, nb.plot = 15, nb.comp = NULL, xmax = NULL,
         beside = TRUE, ...)
}
\arguments{
  \item{x}{gsi object}
  \item{nb.plot}{number of input factors to be considered}
  \item{nb.comp}{number of PCs}
  \item{xmax}{a user-defined maximal \eqn{x} value (\eqn{x\leq 1}) in
    all the bar graphs that show sensitivity indices; or NULL if the user
    wants to keep default values}
  \item{beside}{if TRUE, the main and total sensitivity indices are
    represented by two bars; if FALSE, they are represented by the same bar}
  \item{...}{graphical parameters}
}
\author{M. LAMBONI}
