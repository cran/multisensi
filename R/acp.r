ACP <- function(simuls, inertia=0.95, normalized=TRUE)
{
    ##fait l'ACP

    ##ENTREES
    ## simuls       : sortie des simulations du modèle sous forme d' ARRAY (Y)
    ##                Data.frame output matrix
    ## inertia      : Inertia proportion account by Principal components <1 (0.95 default )
    ##                OR number of PCs to be used (E.g 3)
    ## normalized   : entraine la normalisation de l'ACP
    ##                logical value. TRUE (default) computes a normalized Principal
    ##                Component analysis.

    ##SORTIES
    ## x            : output x of prcomp function (H)
    ## rotation     : the matrix of variable loadings (L)
    ## sdev         : the standard deviations of the principal components
    ## nbcomp       : the number of principal components

    ## ACP (centrée réduite)

    nb.col <- ncol(simuls)
    filtre.var <- NULL
    if(normalized==TRUE){
        filtre.var <- apply(simuls,2,var)==0
        if(all(filtre.var)==TRUE){
            cat("all columns have 0 variance: non-scaled PCA must be done")
            normalized <- FALSE
        }else{
            if(any(filtre.var)==TRUE){
                simuls <- simuls[,!filtre.var]
                print("Constant columns are ignored")
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
  if(inertia >= 1) {nbcomp <- min(inertia, nb.col)}
  ## CASE 2.2 : the user specifies a proportion of inertia
  else {
    nbcomp <- 1
    trouver <- "non"
    while(trouver=="non" & nbcomp < ncol(ACP.prcomp$x)+1){
      if(summary(ACP.prcomp)$importance[3,nbcomp] < inertia){
        nbcomp <- nbcomp + 1
        trouver <- "non"
      } else {
        trouver <- "oui"
      }
    }
  }


    return(list(x=ACP.prcomp$x,
		rotation=ACP.prcomp$rotation,
		sdev=ACP.prcomp$sdev,
		nbcomp=nbcomp))
}

