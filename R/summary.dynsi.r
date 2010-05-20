summary.dynsi <-function(object, ...)
{
    ## object:         dynsi object

    cat("\n Main sensitivity indices \n")
    mSI <- object$mSI
    print( mSI[ rev( order(mSI[,ncol(mSI)]) ) , ], ... )

    cat("\n Total sensitivity indices \n")
    tSI <- object$tSI
    print( tSI[ rev( order(tSI[,ncol(tSI)]) ) , ], ... )

    cat("\n dynsi outputs \n")
    output <- c("X","Y","SI","mSI","tSI","iSI")
    names(output) <- c("Design","response","Indices","Main indices","Total indices","Interaction indices")

    cat("\n value names:", output ,"\n")

}



