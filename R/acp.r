ACP <- function(simuls,normalized=TRUE)
{
    ##fait l'ACP

    ##ENTREES
    ## simuls: sortie des simulations du modèle sous forme d' ARRAY (Y)
    ## normalized: entraine la normalisation de l'ACP

    ##SORTIES
    ## ACP object

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

    ACP <- prcomp(simuls, scale=normalized)

    if(is.null(filtre.var)==FALSE & all(filtre.var)==FALSE & any(filtre.var)==TRUE)
    {
        rotation <- matrix(0,nb.col,ncol(ACP$rotation))

        rotation[!filtre.var,] <- ACP$rotation
        ACP$rotation <-  rotation
    }

    return(ACP)
}

