asg <- function(ANO, ACP, sigma.car, nbcomp=2)
{
    ##ENTREES:
    ##	     ANO	   est un objet ANOVA issu de la sortie de la
    ##                     fonction ACP.ANOVA(objet anova)
    ##	     ACP:	   ACP object
    ##	     sigma.car	   c'est la trace de la matrice (Y'Y)
    ##	     nbcomp	   c'est le nombre de composantes principales
    ##SORTIES
    ##	 indices:			      indices  du premier ordre
    ##	 indices.tot:		      indices total
    ##	 indices.interraction	      indices d'interaction


    ##---------------------------------------------------------------------------
    ## PRELIMINAIRES: recuperation des resultats d'anova
    ##---------------------------------------------------------------------------
    ## aov.df: vecteur des Degres de Liberte, residuelle comprise
    ## aov.ss: vecteur des Sommes de Carres, residuelle comprise
    ## aov.cm: vecteur des Carres Moyens, residuelle comprise
    ## ATTENTION ########
    ## sous S:
    ##aov.summ <- summary(aov.obj)
    ##aov.ss <- aov.summ[,"Sum of Sq"]
    ## sous R:
    aov.summ <- summary( ANO$aov[[1]])[[1]]
    aov.ss <- aov.summ[,"Sum Sq"]
    tab <-as.data.frame(matrix(0,length(aov.ss),nbcomp))
    colnames(tab) <- paste("PC",1:nbcomp,sep="")
    rownames(tab) <-  rownames(aov.summ)
    tab[,1] <- aov.ss

    if(nbcomp>1){
        for (k in 2:nbcomp){     ## pour chaque composante en partant de 2
            aov.summ <- summary( ANO$aov[[k]])[[1]]
            tab[,k] <-aov.summ[,"Sum Sq"]
        }}

    ## ccalcul de la somme des  SC sur  les nbcomp CP pour chaque parametre(SCa).
    if(nbcomp>1){
        somme.ss <- apply(tab, 1, sum)
    } else {somme.ss <- tab[,1]}
    names(somme.ss) <- rownames(summary(ANO$aov[[1]])[[1]])
    tab <- cbind(tab, somme.ss)
    tss <- apply(tab, 2, sum)
    ##	 print(sum(tab[,4]))
    aov.obj <-	ANO$aov[[1]]
    ## indic.fact: matrice 0-1 de correspondance facteurs*termes-du-modele
    indic.fact <- attr(aov.obj$terms,"factors")[-1,]
    filtre.main <- apply(indic.fact,2,sum)==1
    rdf <-  aov.obj$df.residual
    if(rdf>0){ tab <- tab[-nrow(tab),]}
    if(ncol( indic.fact)!=nrow(tab)) { stop("Too small design for estimate all factorial terms")}

    ## calcul du crirere global de la qualite de m'approximation

    ##	  print(sum(tab[,4]))
    ##	  print(sigma.car)
    GC <-(sum(tab[,ncol(tab)]))/sigma.car
    names(GC) <- "GC"
    ##inertie
    inertia <- (c(cumsum(tss[1:nbcomp])/sigma.car, GC))*100

    ##---------------------------------------------------------------------------
    ##affichage des indices de sensibilite
    ##-------------------------------------------------------------------------
    indices <- as.data.frame(matrix(0,nrow(tab),ncol(tab)))
    rownames(indices) <-rownames(tab)
    colnames(indices) <- c( paste("PC",1:(ncol(tab)-1),sep=""),"GSI")

    indices.tot <- as.data.frame(matrix(0,nrow(indic.fact),ncol(tab)))
    rownames( indices.tot) <- rownames(indic.fact)
    colnames( indices.tot) <- c( paste("PC",1:(ncol(tab)-1),sep=""),"GSI")

    indices.inter <-  as.data.frame(matrix(0,nrow(indic.fact),ncol(tab)))
    rownames(indices.inter) <-	rownames(indic.fact)
    colnames(indices.inter) <- c( paste("PC",1:(ncol(tab)-1),sep=""),"GSI")

    indices.main <- indices.tot

    for(k in 1:ncol(tab)){

        indices[,k] <- (tab[,k]/tss[k])*100

        indices.tot[,k] <- ((indic.fact %*%tab[,k])/tss[k])*100

        indices.main[,k] <- ((indic.fact[,filtre.main] %*% tab[,k][filtre.main])/tss[k])*100

        indices.inter[,k] <-  indices.tot[,k]- indices.main[,k]
    }


    ##---------------------------------------------------------------------------------------
    ## calcul de correlations entre les differentes Cp avec les sorties
    ##--------------------------------------------------------------------------------------

    ecart.type <- ACP$sdev[1:nbcomp]
    if(nbcomp==1){
        EcartTypeMat <- matrix(ecart.type,1,1)
    }
    else{
        EcartTypeMat	<- diag( ecart.type[1:nbcomp] )
    }

    correlation <-as.data.frame( ACP$L[,1:nbcomp,drop=FALSE]%*%EcartTypeMat)
    colnames( correlation) <-  paste("PC",1:nbcomp,sep="")

    return(list(SI=indices,
                mSI=indices.main,
                tSI=indices.tot,
                iSI=indices.inter,
                cor=correlation,
                inertia=inertia,
                indic.fact=indic.fact))
}

