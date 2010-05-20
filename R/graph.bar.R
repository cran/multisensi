graph.bar <-function(x, col=1, nb.plot=15,xmax=NULL, beside=TRUE, ...)
{
    ##x :          GSI objects
    ##col :          A number describing the column for S and St
    ##nb.plot:       A number decribing the max number of factor bars to be ploted
    ##xmax:          A number that precise the indice limit

    out <- data.frame(x$tSI[,col],x$mSI[,col])
    rownames(out) <-  rownames(x$tSI)

    out <- out[rev(order(out[,1])),]

    outgraph <- out[rev(1:min(nb.plot,nrow(out))),]
    outgraph <- outgraph/100
    if (is.null(xmax)==TRUE){
        xmax <- 1.1*max(outgraph)
    } else {
        if (xmax>1) {xmax=1}
    }
    outnames <- rownames(outgraph)
    outnames <- gsub("([ ])","",outnames)
    if(beside==FALSE){
        outgraph.2 <- rbind(outgraph[,2],outgraph[,1]-outgraph[,2])
        invisible(
                  barplot(outgraph.2,
                          horiz=TRUE,
                          names.arg=outnames,
                          beside=beside,
                          las=2,
                          cex.axis=2,
                          cex.names=1.5,
                          xlim=c(0,xmax),
                          ...))
    } else {
        outgraph.2 <- rbind(outgraph[,1],outgraph[,2])
        invisible(
                  barplot(outgraph.2,
                          horiz=TRUE,
                          names.arg=outnames,
                          beside=beside,
                          las=2,
                          cex.axis=2,
                          cex.names=1.5,
                          xlim=c(0,xmax),
                          ...))
    }
}

