`yapprox` <- function(ACP, nbcomp=2, aov.obj)
{
    ## calcule les sorties du modele approxime

    ##ENTREES
    ## ACP:        ACP object
    ## annee=NULL:  suppose que l'evaluation sera faite pour toutes les annees
    ## nbcomp:      nombre de facteurs principaux retenus lors de l'ACP
    ## aov.obj:     objet Anova issu de la sortie de la fonction ACP.ANOVA(objet anova)

    ##SORTIES
    ## Y: sorties des Y approximes sous forme de matrice

    ##recuperation des nbcomp premieres lignes de la matrice inversee des vecteurs propres.
    inv.comp <-  t(ACP$L)[1:nbcomp,,drop=FALSE]

    ##calcul des sorties approximees des variables(Y)
    echsimul.app <- aov.obj$PC %*% inv.comp


    return(echsimul.app)
}

