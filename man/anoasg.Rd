\name{anoasg}
\alias{anoasg}
\title{Dynamic main and total sensitivity indices}
\description{
 Function to compute the main and total dynamic sensitivity indices
}
\usage{
anoasg(ANO, nbcomp = 2)
}
\arguments{
  \item{ANO}{anova object obtained from the anovadec function}
  \item{nbcomp}{number of model output to be considered}
}
\value{A list with the following components:
  \describe{
   \item{SI}{data frame of first order, second order, ... indices}
  \item{tSI}{data frame of total sensitivity}
  \item{mSI}{data frame of main sensitivity indices}
  \item{iSI}{data frame of interaction sensitivity indices}
  \item{indic.fact}{data frame of anova object attribute}
  }
}
\note{This is essentially an internal function for the multisensi package}
\author{M. LAMBONI}
