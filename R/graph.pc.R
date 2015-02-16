graph.pc <-function(x, nb.plot=15, nb.comp=NULL, xmax=NULL, beside=TRUE, type="l",...)
{
    ##x :          GSI objects
    ##nb.plot:       A number decribing the max number of factor bars to be ploted

    ## modified HM 1/3/2010 until the end of the function (cf. "toPlot" and "toWrite")
    if(x$normalized){
        toWrite <- "Correlation"
    }
    else{
        toWrite <- "Weighted loadings"
    }

    toPlot <- x$cor
    if(toPlot[floor(nrow(toPlot)/2),1]<0){ toPlot[,1] <- -toPlot[,1]}
    if (is.null(nb.comp)){
        nbcomp <- ncol(toPlot)
    } else {
        nbcomp <- min(nb.comp,ncol(toPlot))
    }

    corrmin <- min(toPlot)
    corrmax <- max(toPlot)
    ## names.comp <- paste("PC",1:nbcomp,sep="")
    inertie <- rep(0,nbcomp)
    inertie[1] <- x$inertia[1]
    if(nbcomp>1){
        for(k in 2:nbcomp) {
            inertie[k] <- x$inertia[k]-x$inertia[k-1]
        }
    }

    if(nbcomp>1){ if(toPlot[1,2]<0){toPlot[,2] <- -toPlot[,2]}}

    names.comp <- paste(paste("PC",1:nbcomp,sep=""),signif(inertie,3),sep=" (" )
    names.comp <- paste(names.comp," %)",sep="")
    main.comp <- names.comp
    par(mfrow=c(2,nbcomp))


    for (k in 1:nbcomp){
        plot(toPlot[,k], ylim=c(corrmin, corrmax) , type=type, col="blue", main= main.comp[k],  ylab=toWrite, lwd=3 ,cex.axis=2 ,cex=4, ...)#xlab="time",
        abline(h=0)
    }

    for(k in 1:nbcomp){
        graph.bar(x ,k ,nb.plot, xmax=xmax, beside=beside, ...)
    }
}

