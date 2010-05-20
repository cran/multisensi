dynsi <- function(formula, model, factors, cumul=FALSE, simulonly=FALSE, nb.outp=NULL, Name.File=NULL, ...)
{
    ##INPUTS
    ## formula      : ANOVA formula like "A+B+c+A:B"   OR  The max interaction
    ##                order like 2 for example.
    ## model        : Data.frame output matrix OR The name of the R-function
    ##                which decribes the model. This function
    ##                must take only a vector corresponding to the input factors values
    ## factors      : Data.frame design if model is data.frame OR a list of
    ##                factors levels: factor<- list(A=c(0,1),B=c(0,1,4))
    ## cumul        : logical value. If TRUE the PCA will be done on the cumalative outputs
    ## simulonly    : logical value.  If TRUE the program simulates the
    ##                model outputs and stops
    ## Name.File    : Name of file containing the R-function model.
    ##                E.g  "exc.ssc"
    ## nb.outp      : number of the first output to be considered if it is not null
    ## ...          : possible fixed parameters of the model function

    ## OUTPUTS
    ## Objet de classe dynsi contenant
    ## X            : data.frame design of experiment (input sample)
    ## Y            : data.frame of model ouput output matrix (response)
    ## S            ; data.frame of first order, two ... Sensitivity Indices (SI) on model outputs
    ## St           : data.frame of total SI on model outputs
    ## Si           : data.frame of principal SI on model outputs
    ## Sinter       : data.frame of interraction SI on model outputs


    ##----------------------------------------------------------------
    ##         Building Design and outputs simulation
    ##----------------------------------------------------------------

    if(is.null(dim(factors))){

        ## Design
        print("Design")
        factors <- planfact.as(factors)
    } else {
        if(is.null(dim(factors))==FALSE & is.data.frame(model)==FALSE){
            ## response simulation
            print("Response simulation")
            model <- simulmodel(model=model, plan=factors, nomFic=Name.File, ...)
            model <- data.frame(model)
            names(model) <-  paste("Y",1:ncol(model),sep="")
        }
    }
    if(cumul==TRUE) { model <- t(apply(model,1,cumsum) )}

    if(simulonly==FALSE){

        if(is.null(nb.outp)==FALSE){nbcomp <- min(nb.outp,ncol(model) ) }
        else {nbcomp <- ncol(model)}

        ##-------------------------------------------------------------
        ##     ANOVAs Analysis on nbcomp sorties
        ##-------------------------------------------------------------
        print("ANOVAs Analysis")
        ANO <- anovadec(model,factors,formula,nbcomp)


        ##--------------------------------------------------------------------
        ##Sensitivity Indices on nbcomp PC and GSA
        ##-------------------------------------------------------------------
        print("Sensitivity Indices")
        ASG <- anoasg(ANO, nbcomp)

        result <- list(X=factors,
                       Y= as.data.frame( model),
                       SI= ASG$SI,
                       mSI=ASG$mSI,
                       tSI= ASG$tSI,
                       iSI= ASG$iSI,
                       Att=ASG$indic.fact)

        class(result) <- "dynsi"
        result
    }
    else{
        return(list(X=factors,Y=model))
    }
}