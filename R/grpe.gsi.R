`grpe.gsi` <- function(GSI,fact.interet)
{
    ## GSI           : GSI object
    ## Fact.interet  ; a vecteur of the names of factors to be grouped.  Eg c("A","B")


    ##     fact.interet1 <- fact.interet
    ## Decomposition entre effets d'interet et autres
    facteurs <- rownames(GSI$Att)
    fact.interet <- apply(outer(facteurs,fact.interet,"=="),1,any)
    if(sum(fact.interet)==1){
        filtre1 <- GSI$Att[fact.interet,]
    } else{
        filtre1 <- apply( GSI$Att[fact.interet,],2,any)
    }
    if(sum(!fact.interet)==1){
        filtre2 <- GSI$Att[!fact.interet,]
    }  else{
        filtre2 <- apply( GSI$Att[!fact.interet,],2,any)
    }
    term1 <- filtre1 & (!filtre2)
    term2 <- (!filtre1) & filtre2
    indic.intrt <- rbind(GSI$Att[!fact.interet,],fact.interet=term1)
    ##     rownames(indic.intrt)[nrow( indic.intrt)] <- quote(fact.interet1)
    intrt.ss <- indic.intrt%*%as.matrix(GSI$SI)

    intrt.ss
}

