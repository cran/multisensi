plot.gsi <-
    function(x, nb.plot=10, nb.comp=3, graph=1:3, xmax=NULL, beside=TRUE, type="l",...)
{

    ## x:         GSI object

    nb.comp <- min(ncol(x$H), nb.comp)
    nb.plot <- min(nb.plot, nrow(x$tSI))

    ##if(is.null(graph)==FALSE && graph>3) {graph <- NULL}

    if(1 %in% graph){
        dev.new();
        ## Correlation graph and main and total sensitivity indices bars on PCs
        graph.pc(x,
                 nb.plot=nb.plot,
                 nb.comp=nb.comp,
                 xmax=xmax,
                 beside=beside,
                 type=type,
                 ...)
    }

    if(2 %in% graph){
        dev.new();
        ## Generalized main and total sensitivity indices bars
        graph.bar(x,
                  ncol(x$tSI),
                  nb.plot,
                  xmax=xmax,
                  beside=beside,
                  ...)
    }

    if(3 %in% graph){
        dev.new();
        ## Dynamic coefficient of determination
      plot(x$Rsquare,
           ylim=c(0,1),
           ylab="Rsquare",
           type=type,
           ...)
    }
}
