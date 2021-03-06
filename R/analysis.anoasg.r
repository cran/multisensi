# Multisensi R package ; file analysis.anoasg.r (last modified: 2017-04-06) 
# Authors: C. Bidot, M. Lamboni, H. Monod
# Copyright INRA 2011-2018 
# MaIAGE, INRA, Univ. Paris-Saclay, 78350 Jouy-en-Josas, France
#
# More about multisensi in https://CRAN.R-project.org/package=multisensi
#
# This software is governed by the CeCILL license under French law and
# abiding by the rules of distribution of free software.  You can  use, 
# modify and/ or redistribute the software under the terms of the CeCILL
# license as circulated by CEA, CNRS and INRIA at the following URL
# "http://www.cecill.info". 
#
# As a counterpart to the access to the source code and  rights to copy,
# modify and redistribute granted by the license, users are provided only
# with a limited warranty  and the software's author,  the holder of the
# economic rights,  and the successive licensors  have only  limited
# liability. 
#
# In this respect, the user's attention is drawn to the risks associated
# with loading,  using,  modifying and/or developing or reproducing the
# software by the user in light of its specific status of free software,
# that may mean  that it is complicated to manipulate,  and  that  also
# therefore means  that it is reserved for developers  and  experienced
# professionals having in-depth computer knowledge. Users are therefore
# encouraged to load and test the software's suitability as regards their
# requirements in conditions enabling the security of their systems and/or 
# data to be ensured and,  more generally, to use and operate it in the 
# same conditions as regards security. 
#
# The fact that you are presently reading this means that you have had
# knowledge of the CeCILL license and that you accept its terms.
#
#===========================================================================
analysis.anoasg <- function(Y, plan,  nbcomp=2, sigma.car=NULL, analysis.args=list(formula=2,keep.outputs=FALSE))
#===========================================================================
{
  ## pour lancer l'utilisation en serie de l'analyse de variance, utilise aov()

  ## INPUTS
  ## Y              : data.frame dont les colonnes sont traitees successivement
  ## plan           : data.frame du design des facteurs (une colonne/facteur)
  ## nbcomp         : nombre de colonnes de Y traitees (les nbcomp premieres)
  ## sigma.car      : permet de choisir de faire les calculs de GSI [dynsi/gsi] et GC/inertia [gsi]
  ##                  = SStot de Y, on fait les calculs de GSI [dynsi/gsi] et GC/inertia [gsi]
  ##                  = NULL sinon, on fait juste les calculs de SI pour chaque colonne
  ## analysis.args  : liste contenant
  ## - formula      : formule pour l'ANOVA "A+B+c+A:B" ou l'ordre max des interactions (entier : 2 par defaut)
  ## - keep.outputs : variable logique pour decider de garder ou non les nbcomp sorties successives

  ## OUTPUTS
  ## SI             : indices de sensibilite des interactions (tableau termes-du-modele*nbcomp)
  ## mSI            : indices de sensibilite principaux (tableau facteurs*nbcomp)
  ## tSI            : indices de sensibilite totaux (tableau facteurs*nbcomp)
  ## iSI            : indices de sensibilite des interactions (tableau facteurs*nbcomp)
  ## inertia        : pourcentages d'inertie expliquee par le modele (vecteur nbcomp+1) [gsi]
  ##                  vide (NULL) si sigma.car=NULL [dynsi]
  ## indic.fact     : matrice 0-1 de correspondance facteurs*termes-du-modele
  ## Hpredict             : contient les sorties de predict(aov_i) pour construire par la suite un metemodele [gsi] (tableau nrow(Y)*nbcomp)
  ##                  vide (NULL) si sigma.car=NULL [dynsi]
  ## outputkept     : liste des nbcomp sorties successives de l'anova (si analysis.args$keep.outputs=TRUE)
  ## call.info        : contient des indications sur la fonction utilisee, call.info$analysis="anova"

  a.args=analysis.args
  # definition des valeurs par defaut des arguments
  if(is.null(a.args$formula)){
    a.args$formula=2;
#    cat("Warning : analysis.anoasg argument 'formula' not given,\n          default value 'formula=2' used.\n")
  }
  if(is.null(a.args$keep.outputs)){
    a.args$keep.outputs=FALSE;
#    cat("Warning : analysis.anoasg argument 'keep.outputs' not given,\n          default value 'keep.outputs=FALSE' used.\n")
  }
  outputkept=NULL#a.args$keep.outputs
  if(a.args$keep.outputs) outputkept=vector("list",nbcomp)


  coordplan <- Y[,1:nbcomp,drop=FALSE]

  PC.names <- colnames(Y)[1:nbcomp];

  ## Mise sous forme de facteurs des variables du plan factoriel
  for (i in 1:ncol(plan)){
    plan[,i] <- as.factor(plan[,i])
    
  }
  
  ## Dataframe necessaire a l'analyse de variance
  Y.data <- data.frame(cbind(plan,coordplan))
  names(Y.data)[seq(from=ncol(plan)+1,to=ncol(plan)+nbcomp,by=1)]=PC.names

  #####################
  ## Formules de l'analyse de variance sur la premiere et la deuxieme composantes principales
  ## Somme des variables dans le cas ou la formule n'est pas donnee.
  ord.inter=a.args$formula;
  if(is.numeric(ord.inter)){
    ## cas ou l'on introduit une valeur numerique pour ord.inter
    form <- paste(names(Y.data[1:ncol(plan)]),collapse="+")
    if(ord.inter==1){
      formule.drt <- form
    }else{
      formule.drt <- paste(paste("(",form,")",sep=""),ord.inter,sep="^")
    }
  }else{           ##cas ou on introduit une formule
    if(is.character(ord.inter)){
      formule.drt <- ord.inter
    }else{
      stop("error in the formula") ## ni formule ni ordre d'interaction introduit
    }
  }
  
  ## indic.fact: matrice 0-1 de correspondance facteurs*termes-du-modele
  indic.fact=attr(terms(formula(paste("~",formule.drt,sep=""))),"factors")
  filtre.main <- colSums(indic.fact)==1 # la ou on a les facteurs seuls (sans interaction)
  # liste des termes factoriels de la formule
  facterms.names <- colnames(indic.fact) # il manque la ligne residus que l'on a en sortie d'aov
  # nb de termes factoriels
  nbterms=ncol(indic.fact)
  # initialisation du tableau de stockage
  # contains the sums of squares of fatorial term i for output j : tab_{w,k}=SS_{w,k}
  # tab au depart de taille (nb de termes factoriels + 1 (residus)) x nbcomp
  tab <-as.data.frame(matrix(NA,nbterms+1,nbcomp))
  colnames(tab) <- PC.names
  rownames(tab) <- c(facterms.names,"Residuals") # nrow(tab)=nb de termes factoriels + 1 (residus)

  #####################
  ## initialisation du vecteur  des sorties de  predictions issues de l'ANOVA
  # utilisees pour le metamodele (fonction yapprox)
  PC.predict <- array(0,dim=c(nrow(Y),nbcomp))
  
  #####################
  ##  Premiere analyse de variance et recuperation de la prediction

  # (1) anova
  formule.tot <- formula(paste( PC.names[1],"~",formule.drt,sep=""))
  Y.1 <- aov(formule.tot,data=Y.data)
  PC.predict[,1] <- predict(Y.1) # pour faire le metamodele
  # (2) construction du tableau avec les SSWk
  aov.summ <- summary(Y.1)[[1]]
  facterms.aov=gsub(" ","",rownames(aov.summ))
  # on stocke les sommes de carres
  tab[facterms.aov,1] <-aov.summ[,"Sum Sq"]
  # on stocke si voulu par utilisateur
  if(a.args$keep.outputs){
    outputkept[[1]]=Y.1
    names(outputkept)[[1]]=paste("aov_",1,sep="")
  }

  # warning 1
  ###########
  ## calcul de la somme des degres de libertes (sauf residus)
  taillemodel=ncol(model.matrix(formula(paste("~",formule.drt,sep="")),data=plan))-1 
  ddlaov=sum(aov.summ$Df)-Y.1$df.residual
  if(ddlaov<taillemodel){
    warning("For some factorial terms there are fewer degrees of freedom than expected from factors specification \n")
  }
  # warning 2
  ###########
  ## si le nb de termes ressortis par l'aov sans les residus ne contient pas tous les termes possibles
  ## grep("Residuals",rownames(aov.summ),invert=TRUE) : liste des termes factoriels retenus par aov sans les résidus
  if (length(grep("Residuals",rownames(aov.summ),invert=TRUE))<nbterms){
    warning("For some factorial terms there is no degree of freedom in the aov table\nToo small design for estimate all factorial terms")
  }

  #####################
  ## Analyses de variance suivantes et recuperation de la prediction













  if(nbcomp>1){
    for(i in 2:nbcomp){ ## pour chaque composante en partant de 2
      # (1) anova
      formule.tot <- formula(paste( PC.names[i],"~",formule.drt,sep=""))
      Y.i <- aov(formule.tot,data=Y.data)
      PC.predict[,i] <- predict(Y.i) # pour faire le metamodele
      # (2) construction du tableau avec les SSWk
      aov.summ <- summary( Y.i)[[1]]
      # on stocke les sommes de carres
      tab[facterms.aov,i] <-aov.summ[,"Sum Sq"]
      # on stocke si voulu par utilisateur
      if(a.args$keep.outputs){
        outputkept[[i]]=Y.i
        names(outputkept)[[i]]=paste("aov_",i,sep="")
      }
    }
  }
  # on enleve le terme Residuals si il existe pour les calculs matriciels d'indices
  # pour ne garder que les termes du modele
  facterms.aov=facterms.aov[grep("Residuals",rownames(aov.summ),invert=TRUE)] 

#########################################################################################################################

  inertia <- NULL;
  if(!is.null(sigma.car)){
    #gsi calculus
    # on ajoute une colonne a tab : tab_{w,end}=\sum_{k=1}^{nbcomp} SS_{w,k}
    ## calcul de la somme des SC sur les nbcomp CP pour chaque parametre(SCa).
    # utile pour calcul GC
    tab <- cbind(tab, rowSums(tab,na.rm=TRUE))
    PC.names <- c(PC.names,"GSI");
    colnames(tab)[nbcomp+1]="GSI";
  }

  # tss vecteur des sommes des carres totales par composantes/sorties
  tss <- colSums(tab,na.rm=TRUE) # dans tss on a les residus

  if(!is.null(sigma.car)){
    #gsi case
    ## calcul du critere global de la qualite de l'approximation
    # ncol(tab)=nbcomp+1
    GC <-(sum(tab[1:nbterms,ncol(tab)],na.rm=TRUE))/sigma.car
    names(GC) <- "GC"
    # dans GC il n'y a pas la residuelle mais dans tss oui
    # tss calculee avant d'enlever une ligne de tab
    ## pourcentages d'inertie
    inertia <- (c(cumsum(tss[1:nbcomp])/sigma.car, GC))*100
  }

  ##---------------------------------------------------------------------------
  ##affichage des indices de sensibilite
  ##-------------------------------------------------------------------------
  indices <- as.data.frame(matrix(NA,nbterms,ncol(tab))) # tab a une taille differente en ligne/colonne suivant les operations precedentes
  rownames(indices) <-facterms.names
  colnames(indices) <- PC.names
  indices.tot <- as.data.frame(matrix(NA,nrow(indic.fact),ncol(tab)))
  rownames( indices.tot) <- rownames(indic.fact)
  colnames( indices.tot) <- PC.names
  indices.inter <- indices.tot
  indices.main <- indices.tot

  for(k in 1:ncol(tab)){
    indices[,k] <- (tab[1:nbterms,k]/tss[k])#*100
  }

  indices.tot=indic.fact[,facterms.aov] %*% as.matrix(indices[facterms.aov,])
  indices.main <- indic.fact[,filtre.main] %*% as.matrix(indices[filtre.main,]); 
  indices.inter <-  indices.tot- indices.main

  ##-----------------------------------------------------------------------------
  ##                                      result
  ##-----------------------------------------------------------------------
  call.info=list(analysis="anova")#,call=match.call())

  return(list(SI=indices,
              mSI=indices.main,
              tSI=indices.tot,
              iSI=indices.inter,
              inertia=inertia,
              indic.fact=indic.fact,
              Hpredict=PC.predict,
              outputkept=outputkept,
              call.info=call.info))
}
