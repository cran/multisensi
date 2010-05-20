\name{asg}
\alias{asg}
\title{Main and total generalized sensitivity computation}
\description{
Function to compute the  main and total generalized sensitivity indices
}
\usage{
asg(ANO, ACP, sigma.car, nbcomp = 2)
}
  \arguments{
  \item{ANO}{ANOVA Object obtained from anovadec function}
  \item{ACP}{ACP object}
  \item{sigma.car}{Inertia for the model output}
  \item{nbcomp}{Numbers of principal component (PC) to be considered}
}
\value{A list with the following components:
  \describe{
  \item{SI}{data frame of first order, second order, ... indices}
  \item{mSI}{data frame of main sensitivity indices}
  \item{tSI}{data frame of total sensitivity}
  \item{iSI}{data frame of interaction sensitivity indices}
  \item{cor}{data frame of correlation between PCs and model output}
  \item{inertia}{Vector of Inertia explained by PCs}
  \item{indic.fact}{data frame of anova object attribute}
  }
}
\note{This is essentially an internal function for the multisensi package}
\author{M. LAMBONI}
