`yapprox` <- function(ACP, nbcomp=2, aov.obj)
{
    ## calcule les sorties du modèle approximé

    ##ENTREES
    ## ACP:        ACP object
    ## annee=NULL:  suppose que l'évaluation sera faite pour toutes les années
    ## nbcomp:      nombre de facteurs principaux retenus lors de l'ACP
    ## aov.obj:     objet Anova issu de la sortie de la fonction ACP.ANOVA(objet anova)

    ##SORTIES
    ## Y: sorties des Y approximés sous forme de matrice

    ##recuperation des nbcomp premières lignes de la matrice inversée des vecteurs propres.
    inv.comp <-  t(ACP$rotation)[1:nbcomp,]

    ##calcul des sorties approximées des variables(Y)
    echsimul.app <- aov.obj$PC %*% inv.comp

    ##Récuperation des Y normalisé selon la procédure de l'ACP
    ynorm <- ACP$x %*% t(ACP$rotation)

    ##calcul de Y'Y=Gsigma
    Gsigma <- t(ynorm) %*% ynorm

    ##calcul de la trace de Gsigma ou de l'inertie des données
    trace <- sum(diag(Gsigma))

    return(list(Y=echsimul.app, trace=trace))
}

