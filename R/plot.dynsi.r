plot.dynsi <- function(x, text.tuning=NULL, shade=FALSE, color=NULL, ...)
{
    ## changed by HM, 2013-05-03 : new arguments "shade" and "color"
    ## ENTREE
    ##               x:         dynsi  object

    ##SORTIE
    ##              Graphique

    filtre.main <- apply(x$Att,2,sum)==1
    filtre.inter <- !filtre.main
    interaction <-  filtre.inter %*%as.matrix(x$SI)
    tab.indicesmi <- as.data.frame(rbind( as.matrix(x$mSI),interaction))
    rownames(tab.indicesmi)<- c(rownames( x$mSI),"interaction")

    indice.cum <-apply( tab.indicesmi,2,cumsum)

    ## recherche des positions pour afficher les noms des parametres

    nb.outp <- ncol(tab.indicesmi)
    names <- rownames(tab.indicesmi)

    n.name <-1:(nrow(tab.indicesmi)+1)

    if(is.null(text.tuning)){ text.tuning=floor(nb.outp/10)+1}
    if(text.tuning> floor(nb.outp/10)){text.tuning=floor(nb.outp/10)+1}

    for(k in 1:nrow(tab.indicesmi)){

        max.k <- max(tab.indicesmi[k,text.tuning:(nb.outp -text.tuning)])

        for(i in text.tuning:(nb.outp-text.tuning)){
            if(max.k==tab.indicesmi[k,i]){ n.name[k] <- i}
        }
    }

    min.inter <- min( indice.cum[nrow(indice.cum),text.tuning:(nb.outp -text.tuning)] )
    for(i in text.tuning:(nb.outp-text.tuning)){
        if(min.inter== indice.cum[nrow(indice.cum),i]){ n.name[length(n.name)] <- i}
    }

    ## changed by HM, 2013-05-03
    if(shade & is.null(color)) densite <- c(7,1,5,10,15,19,23,4) 
    else densite <- rep(NULL, 8) 
    if(!is.null(color)) colset <- color(nrow(tab.indicesmi)) 
    else colset <- rep(NA, nrow(tab.indicesmi)) 

    plot(c(1,nb.outp),
         c(0,0),
         type="l",
         ylab="Indices",
         ylim=c(0,100),
         cex=3,
         ...)
    polygon(c(1,1:nb.outp,nb.outp),
            c(0,indice.cum[1,],0),
            density=densite[1],
            angle=100,
            col=colset[1],
            cex=2)
    text(n.name[1],
         indice.cum[1,n.name[1]]/2,
         names[1],
         cex=1,
         col="black",
         font=2)

    for (k in 2:nrow(tab.indicesmi)){

        j <- k-1
        ## changed by HM, 2013-05-03
        if(shade){
          quat <-floor(j/4)
          if(quat*4-j==0){ang <-0}
          if(-quat*4+j==1){ang <-45}
          if(-quat*4+j==2){ang <-90}
          if(-quat*4+j==3){ang <-135}
        }
        else{ ang <- NULL }

        polygon( c(1:nb.outp, rev(1:nb.outp) ) ,
                c(indice.cum[k-1,],rev(indice.cum[k,])),
                density=densite[ 2+(j-1)%%6 ],angle=ang,
                col=colset[k], border="blue")
        text(n.name[k] ,
             (indice.cum[k-1,n.name[k]]+indice.cum[k,n.name[k]])/2,names[k],
             cex=1,col="black",font=2)
      }
    polygon( c(1:nb.outp, rev(1:nb.outp) ) ,
            c(indice.cum[nrow(indice.cum),],rep(100,nb.outp)),
            density=densite[8], angle=30)
    text(n.name[nrow(tab.indicesmi)+1],
         indice.cum[nrow(indice.cum),n.name[nrow(tab.indicesmi)+1]]/2+50,
         "residual",
         cex=1,col="black",font=2)
}

