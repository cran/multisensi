summary.gsi <- function(object, ...)
{
    ## object:         GSI object

    cat("\n PCs inertia and Global Criterion \n ")
    print(object$inertia, ...)

    cat("\n Main sensitivity indices \n")
    mSI <- object$mSI
    print( mSI[ rev( order(mSI[,ncol(mSI)]) ) , ], ... )

    cat("\n Total sensitivity indices \n")
    tSI <- object$tSI
    print( tSI[ rev( order(tSI[,ncol(tSI)]) ) , ], ... )

    cat("\n gsi outputs \n")
    output <- c("X", "Y", "H", "L", "lambda", "SI", "mSI", "tSI", "iSI", "cor", "inertia", "Rsquare")
    names(output) <- c("Design", "Response", "Principal Components", "Loadings", "PCs variances", "Indices", "First order indices", "Total indices", "Interaction indices", "Correlation", "Inertia", "Rsquare")

    #cat("\n value names:", output ,"\n")
    print(output)
}

