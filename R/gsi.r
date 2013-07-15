gsi <- function(formula, model, factors, inertia=0.95, normalized=TRUE, cumul=FALSE, simulonly=FALSE, Name.File=NULL, ...)
{
  ##INPUTS
  ## formula      : ANOVA formula like "A+B+c+A:B"   OR  The max interaction
  ##                order like 2 for example.
  ## model        : Data.frame output matrix OR The name of the R-function
  ##                which decribes the model. This function
  ##                must take only a vector corresponding to the input factors values
  ## factors      : Data.frame design if model is data.frame OR a list of factors
  ##                levels: factor<- list(A=c(0,1),B=c(0,1,4))
  ## inertia      : Inertia proportion account by Principal components <1 (0.95 default )
  ##                OR number of PCs to be used (E.g 3)
  ## normalized   : logical value. TRUE (default) computes a normalized Principal
  ##                Component analysis.
  ## cumul        : logical value. If TRUE the PCA will be done on the cumulative outputs
  ## simulonly    : logical value.  If TRUE the program simulates only the model outputs
  ##                and stops
  ## Name.File    : Name of file containing the R-function model.
  ##                E.g  "exc.ssc"
  ## ...          : possible fixed parameters of the model function
  
  ## OUTPUTS
  ##GSI objet de classe gsi contient
  ##
  ## X            : data.frame design of experiment (input sample)
  ## Y            : data.frame output matrix (response)
  ## H            :
  ## L            :
  ## lambda       :
  ## inertia      : vector of inertia per PCs and Global criterion
  ## cor          : data.frame of correlation between PCs and outputs
  ## SI           : data.frame of first, two ... order Sensitivity Indices (SI) on PCs and
  ##                        first, two...  order Generalized SI (GSI)
  ## mSI          : data.frame of principal SI on PCs and principal GSI
  ## tSI          : data.frame of total SI on PCs and total GSI
  ## iSI          : data.frame of interaction SI on PCs and interaction GSI
  ## pred         :
  ## residuals    :
  ## Rsquare      : vector of dynamic coefficient of determination
  ## Att          : matrice 0-1 de correspondance facteurs*termes-du-modele
  ## normalized   : logical value used for normalized
  ## cumul        : logical value used for cumul
 
  
  ##----------------------------------------------------------------
  ##  STEP 1 : Build Design if input data is not given
  ##----------------------------------------------------------------
  ## CASE 1 : 'factors' is a list of factor names and levels
  ## CASE 2 : 'factors' is the input dataframe (not tested)
  ##   => if case 1, then a factorial design is constructed
  ##      else, go to STEP 2
  if(is.null(dim(factors))){
    cat("[*] Design \n")
    factors <- planfact.as(factors)
  }

  ##----------------------------------------------------------------
  ##  STEP 2 : Get the simulation outputs
  ##----------------------------------------------------------------
  ## CASE 1 : 'model' is a function
  ## CASE 2 : 'model' is the outputs dataframe (not tested)
  ##   => if case 1, perform the simulations
  ##      else, go to next step
  if(is.data.frame(model)==FALSE){
    ## response simulation
    cat("[*] Response simulation \n")
    model <- simulmodel(model=model, plan=factors, nomFic=Name.File, ...)
  }
   
  ## Optional transformation of the output
  if(cumul==TRUE) { model <- t(apply(model,1,cumsum) )}
  
  ##--------------------------------------------------------------
  ##  STEP 3 : Principal Component Analysis (PCA)
  ##-------------------------------------------------------------
  ## CASE 1 : 'simulonly' is TRUE
  ## CASE 2 : 'simulonly' is FALSE
  ##   => if case 2, get out
  ##      else, perform the PCA
  if(simulonly){
    return(list(X=factors,Y=model))
  }
  
  cat("[*] Principal Component Analysis \n")
  ACP1 <- ACP(model,inertia,normalized)
    
  ##-------------------------------------------------------------
  ##  STEP 4 : ANOVAs Analysis on nbcomp Principal Component (PC)
  ##-------------------------------------------------------------
  cat("[*] ANOVAs Analysis \n")
  ANO <- anovadec(ACP1$H,factors,formula,ACP1$nbcomp)
  
  ##-------------------------------------------------------------
  ##  STEP 5 : Goodness of fit computing
  ##-------------------------------------------------------------
  cat("[*] Goodness of fit computing \n")
  Yapp <- yapprox(ACP1, ACP1$nbcomp, ANO)
  qual.app <- quality(model, Yapp, normalise=normalized)
  
  ##--------------------------------------------------------------------
  ##  STEP 6 ; Sensitivity Indices on nbcomp PC and GSI
  ##-------------------------------------------------------------------
  cat("[*] Sensitivity Indices \n")
  ASG <- asg(ANO, ACP1, ACP1$trace, ACP1$nbcomp)
  
  result <- list(X=factors,
                 Y=as.data.frame(model),
                 H=as.data.frame(ACP1$H[,1:ACP1$nbcomp,drop=FALSE]),
                 L=as.data.frame(ACP1$L[,1:ACP1$nbcomp,drop=FALSE]),
                 lambda=((ACP1$sdev)^2)[1:ACP1$nbcomp],
                 inertia= ASG$inertia,
                 cor=ASG$cor,
                 SI= ASG$SI,
                 mSI=ASG$mSI,
                 tSI= ASG$tSI,
                 iSI= ASG$iSI,
                 pred=as.data.frame(Yapp),
                 residuals=as.data.frame(model)-as.data.frame(Yapp),
                 Rsquare= qual.app$coef.det,
                 Att=ASG$indic.fact,
                 normalized=normalized,
                 cumul=cumul
                 )
  
  class(result) <- "gsi"
  
  result
}

