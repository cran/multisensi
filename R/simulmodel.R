simulmodel <- function(model,plan,nomFic=NULL,verbose=FALSE,...)
{
    ## simulations des sorties du modele selon le plan

    ##ENTREES
    ## nomFic est le nom du fichier ou se trouve le modele
    ##        (A entrer entre guillemets).
    ## model est la fonction renvoyant la sortie du
    ##         modele(Pas de guillemets).
    ## plan est le plan d'experience ou d'echantillonnage sous forme de data.frame


    ##SORTIES
    ##matrice de simulations des sortie sous forme array:  V
    ##Plan de simulations plan sous forme de data.frame : plan

    ## Chargement du fichier contenant le modele

    if(!is.null(nomFic)){ source(nomFic)}

    ## Renvoi de toutes les sorties du modele sous forme de liste.
    ## La taille de la liste est la meme
    ## que celle du plan factoriel, et chaque element de la liste
    ## est la sortie du modele (de la taille
    ## de la serie temporelle renvoyee par le modele).

    U <- NULL

    for (i in 1:nrow(plan)) {
        if( (verbose==TRUE) & ((i%%100)==0) ){ cat(i) }
        U <- c(U,list(model(plan[i,],...)))
    }
    if(verbose){cat("\n")}

    ## Transformation de U (format liste) en V sous forme de tableau pour l'ACP

    U2 <- unlist(U)
    V <- array(U2,dim=c(length(U[[1]]),nrow(plan)))
    V <- t(V)
    V <- as.data.frame(V)
    return(V)
}

