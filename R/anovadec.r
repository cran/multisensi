anovadec <- function(Y, plan, ord.inter, nbcomp=2){
  ##execute l'ANOVA sur chaque CP et recupere la prediction apres l'estimation via ANOVA
  
  ##ENTREES
  ## Y:          : data.frame of model output
  ## plan:         : data.frame of input design
  ## ord.inter:    : prend des valeurs numeriques (1= pas d'interaction);2=interaction
  ##                 d'ordre 2...)  ou une formule alphanumerique souhaitee sous la
  ##                 forme " A+B+C+A*B+A*c+..."
  ##                 par defaut on considere les interactions d'ordre 2.
  ## nbcomp        : nombre de CP sur lesquels s'effectueront les ANOVA
  
  ##SORTIE
  ## Liste contenant les composants
  ##  aov     : liste d'objets aov sur les composantes principales
  ##  PC      : contient la prediction des CP apres l'ANOVA sous forme de ARRAY
  ## Renvoi des coordonnees dans les deux premieres composantes principales
  
  coordplan <- Y[,1:nbcomp,drop=FALSE]
  nomsPC <- paste("PC",1:nbcomp,sep="")
  
  ## Mise sous forme de facteurs des variables du plan factoriel
  for (i in 1:ncol(plan))
    {
      plan[,i] <- as.factor(plan[,i])
      ##        contrasts(plan[,i]) <- contr.poly(nlevels(plan[,i]))
    }
  
  ## Dataframe necessaire a l'analyse de variance
  Y.data <- data.frame(cbind(plan,coordplan))
  names(Y.data)[seq(from=ncol(plan)+1,to=ncol(plan)+nbcomp,by=1)]=nomsPC

  ## Formules de l'analyse de variance sur la premiere et la deuxieme composantes principales
  
  ## Somme des variables dans le cas ou la formule n'est pas donnee.
  if(is.numeric(ord.inter)){
    ## cas ou l'on introduit une valeur numerique pour ord.inter
    form <- paste(names(Y.data[1:ncol(plan)]),collapse="+")
    if(ord.inter==1){
      formule.drt <- form
    } else{
      formule.drt <- paste(paste("(",form,")",sep=""),ord.inter,sep="^")
    }
  } else{           ##cas ou on introduit une formule
    if(is.character(ord.inter)){
      formule.drt <- ord.inter
    } else{
      stop("error in the formula") ## ni formule ni ordre d'interaction introduit
    }
  }
  
  ## initialisation du vecteur  des sorties de  predictions issues de l'ANOVA
  PC.predict <- array(0,dim=c(length(Y[,1]),nbcomp))
  
  ## Analyses de variance et recuperation de la prediction
  YaovPC <- list()
  for(i in 1:nbcomp)
    {
      formule.tot <- formula(paste( nomsPC[i],"~",formule.drt,sep=""))
      Y.i <- aov(formule.tot,data=Y.data)
      PC.predict[,i] <- predict(Y.i)
      YaovPC <- c(YaovPC, list(Y.i))
    }
  
  ## Renvoi des objets aov issus des deux analyses
  
  return( list(aov=YaovPC, PC=PC.predict) )
}

