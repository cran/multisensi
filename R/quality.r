`quality` <- function(echsimul,echsimul.app,normalise=TRUE)
{
    ## calcul le biais , le coef. de determination, le lof

    ##ENTREES:
    ## echsimul:      ARRAY des sorties ou matrice des sorties V
    ## echsimul.app:  sortie du modele approxime (ces variables sont issus des Y centrees et  reduites)

    ##SORTIES:
    ##moy.biais:       biais integre
    ##coef.det:        coefficient de determination



    ##  calcul de la moyenne de Y initiales
    moy.Y <- apply(echsimul,2,mean)

    ##moyenne de Y sous forme matricielle
    moy.Ymat <- matrix(1,length(echsimul.app[,1]),1) %*%moy.Y

    ## ecart type de Y initiales (non normalise).
    sdv.Y <-sqrt( apply(echsimul,2,var) )

    ## reconstitution des  valeurs de Yapp du fait que les Y utilises pour ACP etaient reduits

    if (normalise==TRUE){
        for (j in 1:length(echsimul.app[1,])) {
            echsimul.app[,j] <-  echsimul.app[,j]* sdv.Y[j] }}
    ## decentre
    echsimul.app <-  echsimul.app+ moy.Ymat

    ## calcul du biais
    biais <- echsimul-echsimul.app
    moy.biais <- apply(biais,2,mean)

    ##calcul du coeficient de determination

    coef.det <- apply((echsimul.app-moy.Ymat)^2,2,sum)/apply((echsimul-moy.Ymat)^2,2,sum)

    ## renvoi des sorties
    return(list(moy.biais=moy.biais,coef.det=coef.det))
  }

