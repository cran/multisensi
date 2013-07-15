ACP <- function(simuls, dimension=0.95, normalized=TRUE)
{
    ##fait l'ACP

    ##ENTREES
    ## simuls       : sortie des simulations du modele sous forme d' ARRAY (Y)
    ##                Data.frame output matrix
    ## dimension    : Inertia proportion account by Principal components <1 (0.95 default )
    ##                OR number of PCs to be used (E.g 3)
    ## normalized   : entraine la normalisation de l'ACP
    ##                logical value. TRUE (default) computes a normalized Principal
    ##                Component analysis.

    ##SORTIES
    ## H            : output x of prcomp function
    ## L	    : the matrix of variable loadings - output rotation of prcomp function
    ## sdev         : the standard deviations of the principal components
    ## nbcomp       : the number of principal components
    ## trace

    ## ACP (centree reduite)

    nb.col <- ncol(simuls)
    filtre.var <- NULL
    if(normalized==TRUE){
        filtre.var <- apply(simuls,2,var)==0
        if(all(filtre.var)==TRUE){
            cat("all columns have 0 variance: non-scaled PCA must be done \n")
            normalized <- FALSE
        }else{
            if(any(filtre.var)==TRUE){
                simuls <- simuls[,!filtre.var]
                cat("Constant columns are ignored \n")
            }
        }
    }

    ACP.prcomp <- prcomp(simuls, scale=normalized)

    if(is.null(filtre.var)==FALSE & all(filtre.var)==FALSE & any(filtre.var)==TRUE)
    {
        rotation <- matrix(0,nb.col,ncol(ACP.prcomp$rotation))

        rotation[!filtre.var,] <- ACP.prcomp$rotation
        ACP.prcomp$rotation <-  rotation
    }

  ## Selection of the principal components
  ## CASE 2.1 : the user specifies the number of PCs to keep
  if(dimension >= 1) {nbcomp <- min(dimension, nb.col)}
  ## CASE 2.2 : the user specifies a proportion of inertia
  else {
    nbcomp <- match(TRUE,summary(ACP.prcomp)$importance[3,]>=dimension)
    ## importance = cumsum(ACP.prcomp$sdev^2/sum(ACP.prcomp$sdev^2))
    #nbcomp <- 1
    #trouver <- "non"
    #while(trouver=="non" & nbcomp < ncol(ACP.prcomp$x)+1){
    #  if(summary(ACP.prcomp)$importance[3,nbcomp] < dimension){
    #    nbcomp <- nbcomp + 1
    #    trouver <- "non"
    #  } else {
    #    trouver <- "oui"
    #  }
    #}
  }

  ##Recuperation des Y normalise selon la procedure de l'ACP
  ynorm <- ACP.prcomp$x %*% t(ACP.prcomp$rotation)
  ##calcul de Y'Y=Gsigma
  Gsigma <- t(ynorm) %*% ynorm
  ##calcul de la trace de Gsigma ou de l'inertie des donnees
  trace <- sum(diag(Gsigma))

    return(list(H=ACP.prcomp$x,
		L=ACP.prcomp$rotation,
		sdev=ACP.prcomp$sdev,
		nbcomp=nbcomp,
		trace=trace))
}

