`planfact.as` <- function(input)
{
    ##genere le plan factoriel complet en utilisant les vraies
    ##       modalites des parametres

    ##ENTREES
    ## input      est une liste d'elements contenant les modalites
    ##            des differents parametres


    ##SORTIES
    ## plan factoriel complet sous forme de data frame contenant
    ## les vraies modalites des inputs

    nb.niv <-1:length(input)
    for (k in 1:length(nb.niv)) {
        nb.niv[k] <- length(input[[k]])
    }
    ## Creation du plan factoriel
    plan <- planfact(nb.niv,make.factor=FALSE)
    ## Recodage des facteurs avec les valeurs transmises dans input

    echSimul <- matrix(NA,nrow(plan),ncol(plan))

    for (j in 1:ncol(plan)) {
        tmp <- input[[j]] ## modalites du facteur j
        plj <- plan[,j]+1   ## niveaux du facteur j dans le plan
        tmp <- tmp[plj]
        echSimul[,j] <- tmp

    }


    ## Recuperation des noms des variables

    if( is.null(names(input))) {  colnames(echSimul) <- colnames(plan)}
    else {                     colnames(echSimul) <- names(input[1:ncol(plan)])}

    ## Transformation du plan en dataframe
    echSimul <- as.data.frame(echSimul)

    ## Renvoi de l'echantillon
    return(echSimul)
}

