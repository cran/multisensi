`yapprox` <- function(ACP, nbcomp=2, aov.obj)
{
    ## calcule les sorties du mod�le approxim�

    ##ENTREES
    ## ACP:        ACP object
    ## annee=NULL:  suppose que l'�valuation sera faite pour toutes les ann�es
    ## nbcomp:      nombre de facteurs principaux retenus lors de l'ACP
    ## aov.obj:     objet Anova issu de la sortie de la fonction ACP.ANOVA(objet anova)

    ##SORTIES
    ## Y: sorties des Y approxim�s sous forme de matrice

    ##recuperation des nbcomp premi�res lignes de la matrice invers�e des vecteurs propres.
    inv.comp <-  t(ACP$rotation)[1:nbcomp,]

    ##calcul des sorties approxim�es des variables(Y)
    echsimul.app <- aov.obj$PC %*% inv.comp

    ##R�cuperation des Y normalis� selon la proc�dure de l'ACP
    ynorm <- ACP$x %*% t(ACP$rotation)

    ##calcul de Y'Y=Gsigma
    Gsigma <- t(ynorm) %*% ynorm

    ##calcul de la trace de Gsigma ou de l'inertie des donn�es
    trace <- sum(diag(Gsigma))

    return(list(Y=echsimul.app, trace=trace))
}

