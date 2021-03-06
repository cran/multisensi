\name{multisensi}
\alias{multisensi}
\title{
  A function with multiple options to perform multivariate sensitivity analysis
}
\description{
  The \code{multisensi} function can conduct the different steps of a multivariate sensitivity analysis (design, simulation, dimension reduction, analysis, plots). It includes different options for each of these steps. 
}
\usage{
multisensi(design = expand.grid, model, reduction = basis.ACP, 
           dimension = 0.95, center = TRUE, scale = TRUE, 
           analysis = analysis.anoasg, cumul = FALSE, 
           simulonly = FALSE, Name.File = NULL, 
           design.args = list(), basis.args = list(), 
           analysis.args = list(), ...)
}
\arguments{
  \item{design}{EITHER a function such as expand.grid to generate the design OR a data.frame of size \code{N x P} containing \code{N} combinations of levels of the \code{P} input factors OR a function from the \pkg{sensitivity} package such as fast99 OR an object generated by a function from the \pkg{sensitivity} package. The first and third cases require additional information to be given in the \code{design.args} argument. 
  }
  \item{model}{EITHER a function to run the model simulations OR a data.frame of size \code{N x T} containing \code{N} realizations of \code{T} output variables.
  }
  \item{reduction}{EITHER a function to decompose the multivariate output on a basis of smaller dimension OR \code{NULL}. The first case requires additional information to be given in the \code{basis.args} argument. In the second case, sensitivity analyses are performed on the raw output variables.
}
  \item{dimension}{EITHER the number of variables to analyse, specified by an integer or by the minimal proportion of inertia (a scalar < 1) to keep in the output decomposition OR a vector specifying a subset of columns in the output data.frame OR NULL if all variables must be analysed. 
  }
  \item{center}{logical value. If TRUE (default value) the output variables are centred.
  }
  \item{scale}{logical value. If TRUE (default value) the output variables are normalized before applying the reduction function.
  }
  \item{analysis}{a function to run the sensitivity analysis. Additional information can be given in the \code{analysis.args} argument. 
  }
  \item{cumul}{logical value. If TRUE the output variables are replaced by their cumulative sums.
  }
  \item{simulonly}{logical value.  If TRUE the program stops after the model simulations.
  }
  \item{Name.File}{Name of file containing the R-function model.
  }
  \item{design.args}{a list of arguments for the function possibly given in the \code{design} argument.
  }
  \item{basis.args}{a list of arguments for the function given in the \code{reduction} argument. See the function help for more precision.
%    - NULL              | multisensi <-> dynsi
%      - basis.ACP         | basis.args=list()
%      - basis.bsplines    | basis.args=list(knots=5, mdegree=3, [x.coord=1:N facultatif]) , knots peut etre un vecteur (position des noeuds)
%      - basis.osplines    | basis.args=list(knots=5, mdegree=3, [x.coord=1:N facultatif]) , knots peut etre un vecteur (position des noeuds)
%      - basis.mine        | basis.args=list(baseL = une matrice de T lignes donnant les (coordonnees des) vecteurs de la base choisie pour la reduction)
}
  \item{analysis.args}{a list of arguments for the function possibly given in the \code{analysis} argument. See the function help for more precision.
%      - analysis.anoasg      | analysis.args=list(formula=2, [keep.outputs=FALSE facultatif]) formula=modele/formule sur les facteurs a fournir pour l'ANOVA
%      - analysis.sensitivity | analysis.args=list([keep.outputs=FALSE facultatif])
}
  \item{\dots}{optional parameters of the function possibly given in the \code{model} argument.
}
}
\value{
  an object of class \code{\link{dynsi}} if \code{reduction=NULL}, otherwise an object of class \code{\link{gsi}}. See the functions \code{\link{dynsi}}  and \code{\link{gsi}} for more information.
%OPTION DYNSI (reduction=NULL)
%  ## Objet de classe dynsi contenant
%   X            : data.frame design of experiment (input sample)
%   Y            : data.frame of model ouput output matrix (response)
%   SI           : data.frame of first order, two ... Sensitivity Indices (SI) on model outputs
%   tSI          : data.frame of total SI on model outputs
%   mSI          : data.frame of principal SI on model outputs
%   iSI          : data.frame of interaction SI on model outputs
%   Att          : 
%  
%OPTION GSI 
%  ## Objet de classe gsi contenant
%   X            : data.frame design of experiment (input sample)
%   Y            : data.frame output matrix (response)
%   H            :
%   L            :
%   lambda       :
%   inertia      : vector of inertia per PCs and Global criterion
%   cor          : data.frame of correlation between PCs and outputs
%   SI           : data.frame of first, two ... order Sensitivity Indices (SI) on PCs and
%                          first, two...  order Generalised SI (GSI)
%   mSI          : data.frame of principal SI on PCs and principal GSI
%   tSI          : data.frame of total SI on PCs and total GSI
%   iSI          : data.frame of interaction SI on PCs and interaction GSI
%   pred         :
%   residuals    :
%   Rsquare      : vector of dynamic coefficient of determination
%   Att          : matrice 0-1 de correspondance facteurs*termes-du-modele
%   scale   : logical value used for scale
%   cumul        : logical value used for cumul

}
%\author{C. Bidot, H Monod}


\seealso{\code{\link{dynsi}}, \code{\link{gsi}}}

\examples{
## Test case : the Winter Wheat Dynamic Models (WWDM)
#  input factors design
 data(biomasseX)
# input climate variable
 data(Climat)
# output variables (precalculated to speed up the example)
 data(biomasseY)

# to do dynsi process
# argument reduction=NULL
resD <- multisensi(design=biomasseX, model=biomasseY, reduction=NULL,
                 dimension=NULL, analysis=analysis.anoasg,
                 analysis.args=list(formula=2,keep.outputs = FALSE))
summary(resD)


# to do gsi process
#------------
# with dimension reduction by PCA
# argument reduction=basis.ACP
resG1 <- multisensi(design=biomasseX, model=biomasseY, reduction=basis.ACP,
                   dimension=0.95, analysis=analysis.anoasg,
                   analysis.args=list(formula=2,keep.outputs = FALSE))
summary(resG1)

plot(x=resG1, beside=FALSE)

#------------
# with dimension reduction by o-splines basis
# arguments reduction=basis.osplines
# and basis.args=list(knots= ... , mdegree= ... )

resG2 <- multisensi(design=biomasseX, model=biomasseY, reduction=basis.osplines,
                   dimension=NULL, center=FALSE, scale=FALSE,
                   basis.args=list(knots=11, mdegree=3), analysis=analysis.anoasg, 
                   analysis.args=list(formula=2,keep.outputs = FALSE))
summary(resG2)

#------------
library(sensitivity) # to use fast99

# with dimension reduction by o-splines basis
# and sensitivity analysis with sensitivity:fast99
resG3 <- multisensi(design=fast99, model=biomasse,
                   analysis=analysis.sensitivity, 
                   design.args=list(factors = names(biomasseX), n = 100,
                   q = "qunif", q.arg = list(list(min = 0.9, max = 2.8),
                   list(min = 0.9, max = 0.99), list(min = 0.6, max = 0.8), 
                   list(min = 3, max = 12), list(min = 0.0035, max = 0.01),
                   list(min = 0.0011, max = 0.0025),
                   list(min = 700, max = 1100))), climdata=Climat,
                   reduction=basis.osplines,
                   basis.args=list(knots=7, mdegree=3),
                   center=FALSE,scale=FALSE,dimension=NULL)
summary(resG3)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate data}
\keyword{sensitivity analysis}
