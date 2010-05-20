`print.gsi` <- function(x, ...)
{
    ## x:         GSI object

    cat("\n sensitivity indices \n")
    print(x$SI[rev(order(x$SI[,ncol(x$SI)])),], ...)
    cat("\n Total sensitivity indices \n")
    print(x$tSI[rev(order(x$tSI[,ncol(x$tSI)])),], ...)
    invisible(x)
}

