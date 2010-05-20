plot.dynsi <- function(x, text.tuning=NULL, ...)
{
    ## ENTREE
    ##               x:         dynsi  object

    ##SORTIE
    ##              Graphique

    filtre.main <- apply(x$Att,2,sum)==1
    filtre.inter <- !filtre.main
    interaction <-  filtre.inter %*%as.matrix(x$SI)
    x$iSI <- as.data.frame(rbind( as.matrix(x$iSI),interaction))
    rownames(x$iSI)<- c(rownames( x$tSI),"interaction")

    indice.cum <-apply( x$iSI,2,cumsum)

    ## recherche des postions pour afficher les noms des parametres

    n.name <-1:(nrow(x$iSI)+1)

    if(is.null(text.tuning)){ text.tuning=floor(ncol(x$iSI)/10)+1}
    if(text.tuning> floor(ncol(x$iSI)/10)){text.tuning=floor(ncol(x$iSI)/10)+1}

    for(k in 1:nrow(x$iSI)){

        max.k <- max(x$iSI[k,text.tuning:(ncol(x$iSI) -text.tuning)])

        for(i in text.tuning:(ncol(x$iSI)-text.tuning)){
            if(max.k==x$iSI[k,i]){ n.name[k] <- i}
        }
    }

    min.inter <- min( indice.cum[nrow(indice.cum),text.tuning:(ncol(x$iSI) -text.tuning)] )
    for(i in text.tuning:(ncol(x$iSI)-text.tuning)){
        if(min.inter== indice.cum[nrow(indice.cum),i]){ n.name[length(n.name)] <- i}
    }

    nb.outp <- ncol(x$iSI)
    names <- rownames(x$iSI)

    densite <- c(1,5,10,15,19,23)
    ##angle <- c(0,45,90,135)


    plot(c(1,nb.outp),
         c(0,0),
         type="l",
         ylab="Indices",
         ylim=c(0,100),
         cex=3,
         ...)
    polygon(c(1,1:nb.outp,nb.outp),
            c(0,indice.cum[1,],0),
            density=7,
            angle=100,
            cex=2)

    text(n.name[1],
         indice.cum[1,n.name[1]]/2,
         names[1],
         cex=1,
         col="black",
         font=2)

    for (k in 2:nrow(x$iSI)){

        j <- k-1
        if(j<=6){ dens <- densite[j]}
        else{
            if(j<=12){dens <- densite[j-6]}
            else{
                if(j<=18){dens <- densite[j-12]}
                else{
                    if(j<=24){ dens <- densite[j-18]}
                    else{ den <- 30

                      }
                }
            }
        }
        quat <-floor(j/4)
        if(quat*4-j==0){ang <-0}
        if(-quat*4+j==1){ang <-45}
        if(-quat*4+j==2){ang <-90}
        if(-quat*4+j==3){ang <-135}

        polygon( c(1:nb.outp, rev(1:nb.outp) ) ,
                c(indice.cum[k-1,],rev(indice.cum[k,])),
                density=dens,angle=ang,border="blue")
        text(n.name[k] ,
             (indice.cum[k-1,n.name[k]]+indice.cum[k,n.name[k]])/2,names[k],
             cex=1,col="black",font=2)
    }
    polygon( c(1:nb.outp, rev(1:nb.outp) ) ,
            c(indice.cum[nrow(indice.cum),],rep(100,nb.outp)),
            density=4, angle=30)
    text(n.name[nrow(x$iSI)+1],
         indice.cum[nrow(indice.cum),n.name[nrow(x$iSI)+1]]/2+50,
         "residual",
         cex=1,col="black",font=2)
}

