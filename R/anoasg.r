anoasg <- function(ANO,nbcomp=2)
{

    ##ENTREES:
    ##   ANO    est un objet ANOVA issu de la sortie de la fonction ACP.ANOVA(objet anova)
    ##   nbcomp c'est le nombre de composantes principales

    ##SORTIES
    ##             indices:                         indices  du premier ordre
    ##             indices.tot:                     indices total
    ##             indices.interraction             indices d'interaction


    ##---------------------------------------------------------------------------
    ## PRELIMINAIRES: récupération des résultats d'anova
    ##---------------------------------------------------------------------------
    ## aov.df: vecteur des Degrés de Liberté, résiduelle comprise
    ## aov.ss: vecteur des Sommes de Carrés, résiduelle comprise
    ## aov.cm: vecteur des Carrés Moyens, résiduelle comprise
######## ATTENTION ########
    ## sous S:
    ##aov.summ <- summary(aov.obj)
    ##aov.ss <- aov.summ[,"Sum of Sq"]
    ## sous R:

    aov.summ <- summary( ANO$aov[[1]])[[1]]
    aov.ss <- aov.summ[,"Sum Sq"]
    tab <-as.data.frame( matrix(0,length(aov.ss),nbcomp))
    colnames(tab) <- paste("Y",1:nbcomp,sep="")
    rownames(tab) <-  rownames(aov.summ)
    tab[,1] <- aov.ss

    if(nbcomp>1){
        for (k in 2:nbcomp){     ## pour chaque composante en partant de 2
            aov.summ <- summary( ANO$aov[[k]])[[1]]
            tab[,k] <-aov.summ[,"Sum Sq"]
        }}

    tss <- apply(tab, 2, sum)

    aov.obj <-  ANO$aov[[1]]
    ## indic.fact: matrice 0-1 de correspondance facteurs*termes-du-modèle
    indic.fact <- attr(aov.obj$terms,"factors")[-1,]
    filtre.main <- apply(indic.fact,2,sum)==1
    rdf <-  aov.obj$df.residual
    if(rdf>0){ tab <- tab[-nrow(tab),]}
    if(ncol( indic.fact)!=nrow(tab)) { stop("Too small  design for estimate all factorial terms")}

    ##---------------------------------------------------------------------------
    ##affichage des indices de sensibilité
    ##-------------------------------------------------------------------------
    indices <- as.data.frame(matrix(0,nrow(tab),ncol(tab)))
    rownames(indices) <-rownames(tab)
    colnames(indices) <-  paste("Y",1:(ncol(tab)),sep="")

    indices.tot <- as.data.frame(matrix(0,nrow(indic.fact),ncol(tab)))
    rownames( indices.tot) <- rownames(indic.fact)
    colnames( indices.tot) <-  colnames(indices)

    indices.main <- indices.tot

    indices.inter <-  as.data.frame(matrix(0,nrow(indic.fact),ncol(tab)))
    rownames(indices.inter) <-  rownames(indic.fact)
    colnames(indices.inter) <- colnames(indices)

    for(k in 1:ncol(tab)){

        indices[,k] <- (tab[,k]/tss[k])*100

        indices.tot[,k] <- ((indic.fact %*%tab[,k])/tss[k])*100

        indices.main[,k] <- ((indic.fact[,filtre.main] %*% tab[,k][filtre.main])/tss[k])*100

        indices.inter[,k] <-  indices.tot[,k]- indices.main[,k]
    }
    ##-----------------------------------------------------------------------------
    ##                                      result
    ##-----------------------------------------------------------------------

    return(list(SI=indices,
                mSI=indices.main,
                tSI=indices.tot,
                iSI=indices.inter,
                indic.fact=indic.fact))
}

