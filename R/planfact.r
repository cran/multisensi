`planfact` <- function(nb.niv,make.factor=TRUE)
{
    ## génère un plan factoriel complet dans l'ordre lexicographique
    ## ENTREES
    ## nb.niv: vecteur composé des nombres de niveaux de chaque facteur
    ## SORTIE
    ## un "dataframe" dont les colonnes sont des facteurs au sens Splus

    N <- prod(nb.niv)

    n <- length(nb.niv)

    plan <- matrix(NA,N,n)
    plan <- as.data.frame(plan)

    for(i in 1:(n-1))
    {
        plan[,i] <- rep( 1:nb.niv[i],  rep( prod(nb.niv[(i+1):n]), nb.niv[i] ))

        if(make.factor==TRUE){

            plan[,i] <- factor(plan[,i])

        }
    }

    plan[,n] <- rep( 1:nb.niv[n],  prod(nb.niv[1:(n-1)])   )

    if(make.factor==TRUE){plan[,n] <- factor(plan[,n])}

    if(make.factor==FALSE){plan <- plan-1}

    plan <- as.data.frame(plan)
    names(plan) <- paste("V",1:n,sep="")

    plan
}

