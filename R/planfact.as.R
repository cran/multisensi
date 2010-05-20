`planfact.as` <- function(input)
{
    ##génère le plan factoriel complet en utilisant les vraies
    ##       modalités des paramètres

    ##ENTREES
    ## input      est une liste d'élements contenant les modalités
    ##            des différents paramètres


    ##SORTIES
    ## plan factoriel complet sous forme de data frame contenant
    ## les vraies modalités des inputs

    nb.niv <-1:length(input)
    for (k in 1:length(nb.niv)) {
        nb.niv[k] <- length(input[[k]])
    }
    ## Création du plan factoriel
    plan <- planfact(nb.niv,make.factor=FALSE)
    ## Recodage des facteurs avec les valeurs transmises dans input

    echSimul <- matrix(NA,nrow(plan),ncol(plan))

    for (j in 1:ncol(plan)) {
        tmp <- input[[j]] ## modalités du facteur j
        plj <- plan[,j]+1   ## niveaux du facteur j dans le plan
        tmp <- tmp[plj]
        echSimul[,j] <- tmp

    }


    ## Récupération des noms des variables

    if( is.null(names(input))) {  colnames(echSimul) <- colnames(plan)}
    else {                     colnames(echSimul) <- names(input[1:ncol(plan)])}

    ## Transformation du plan en dataframe
    echSimul <- as.data.frame(echSimul)

    ## Renvoi de l'échantillon
    return(echSimul)
}

