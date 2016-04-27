\name{predict.gsi}
\alias{predict.gsi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to predict multivariate output 
}
\description{
The function \code{predict.gsi} generates predicted multivariate output for user-specified combinations of levels of the input factors.
}
\usage{
\method{predict}{gsi}(object, newdata, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{Object of class gsi.}
  \item{newdata}{An optional data frame in which to look for variables with
         which to predict. If omitted, the fitted values are used.
need to be same factors and levels as for obtained the gsi object.}
  \item{...}{others parameters}

}
\value{ a data.frame of predicted values for newdata}
\details{Only available if the \code{gsi} object was obtained with analysis.anoasg and analysis.args$keep.outputs=TRUE.}
\author{C. Bidot, H.Monod}

\seealso{\code{\link{gsi}}, \code{\link{multisensi}}, \code{\link{analysis.anoasg}}}
\examples{
  data(biomasseX)
  data(biomasseY)
  x=multisensi(design=biomasseX,model=biomasseY,basis=basis.ACP,
               analysis=analysis.anoasg,
               analysis.args=list(formula=2, keep.outputs=TRUE))
  newdata=as.data.frame(apply(biomasseX,2,unique))
  predict(x,newdata)
}

