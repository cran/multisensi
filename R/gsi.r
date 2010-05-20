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
    ## cumul        : logical value. If TRUE the PCA will be done on the cumalative outputs
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
    ## S            ; data.frame of first, two ... order Sensitivity Indices (SI) on PCs and
    ##                        first, two...  order Generalized SI (GSI)
    ## St           : data.frame of total SI on PCs and total GSI
    ## Si           : data.frame of principal SI on PCs and principal GSI
    ## Sinter       : data.frame of interraction SI on PCs and interraction GSI
    ## Cor          : data.frame of correlation between PCs and outputs
    ## Inertia           : vector of inertia per PCs and Global criterion
    ## Rsquare      : vector of dynamic coefficient of determination



    ##----------------------------------------------------------------
    ##         Building Design and outputs simulation
    ##----------------------------------------------------------------
    if(is.null(dim(factors))){

        ## Design
        print("Design")
        factors <- planfact.as(factors)
    }

    if(is.null(dim(factors))==FALSE & is.data.frame(model)==FALSE){
        ## response simulation
        print("Response simulation")
        model <- simulmodel(model=model, plan=factors, nomFic=Name.File, ...)

    }

    if(cumul==TRUE) { model <- t(apply(model,1,cumsum) )}

    if(simulonly==FALSE){
        ##--------------------------------------------------------------
        ##         Principal Component Analysis (PCA)
        ##-------------------------------------------------------------
        print("Principal Component Analysis")

        ACP1 <- ACP(model,normalized)

        ## recherche du nombre CP
        if(inertia>=1) {nbcomp <- min(inertia, ncol(model))}
        else {
            nbcomp <- 1
            trouver <- "non"
            while(trouver=="non" & nbcomp< ncol(ACP1$x)+1){
                if(summary(ACP1)$importance[3,nbcomp]<inertia){
                    nbcomp <- nbcomp+1
                    trouver <- "non"
                } else {
                    trouver <- "oui"
                }
            }
        }
        ##-------------------------------------------------------------
        ##     ANOVAs Analysis on nbcomp Principal Component (PC)
        ##-------------------------------------------------------------
        print("ANOVAs Analysis")
        ANO <- anovadec(ACP1$x,factors,formula,nbcomp)

        ##-------------------------------------------------------------
        ##        Goodness of fit computing
        ##-------------------------------------------------------------
        print("Goodness of fit computing")
        Yapp <- yapprox(ACP1, nbcomp, ANO)
        qual.app <- quality(model, Yapp$Y, normalise=normalized)

        ##--------------------------------------------------------------------
        ##Sensitivity Indices on nbcomp PC and GSI
        ##-------------------------------------------------------------------
        print("Sensitivity Indices")
        ASG <- asg(ANO, ACP1, Yapp$trace, nbcomp)

        result <- list(X=factors,
                       Y=as.data.frame(model),
                       H=as.data.frame(ACP1$x[,1:nbcomp]),
                       L=as.data.frame(ACP1$rotation[,1:nbcomp]),
                       lambda=((ACP1$sdev)^2)[1:nbcomp],
                       inertia= ASG$inertia,
                       cor=ASG$cor,
                       SI= ASG$SI,
                       mSI=ASG$mSI,
                       tSI= ASG$tSI,
                       iSI= ASG$iSI,
                       pred=as.data.frame(Yapp$Y),
                       residuals=as.data.frame(model)-as.data.frame(Yapp$Y),
                       Rsquare= qual.app$coef.det,
                       Att=ASG$indic.fact,
                       normalized=normalized,
                       cumul=cumul
                       )

        class(result) <- "gsi"

        result
    } else{
        return(list(X=factors,Y=model))
    }

}

