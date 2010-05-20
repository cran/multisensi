#===========================================================================
biomasse <- function(input , climdata, annee=3)
#===========================================================================
  {
    if(class(input)=="numeric"){input <- as.data.frame(as.list(input))}
    Eb <-    input[1,1]
    Eimax <- input[1,2]
    K <-     input[1,3]
    Lmax <-  input[1,4]
    A <-     input[1,5]
    B <-     input[1,6]
    TI <-    input[1,7]
    if (is.null(annee)){
           annee <- input[1,8]
    }


    #Calcul de PAR et de ST a partir des fichiers climatiques
    PAR<-0.5*0.01*climdata$RG[climdata$ANNEE==annee]
    Tmoy<-(climdata$Tmin[climdata$ANNEE==annee]+
           climdata$Tmax[climdata$ANNEE==annee])/2
    Tmoy[Tmoy<0]<-0
    ST<-Tmoy
    for (i in (2:length(Tmoy)))
      {
        ST[i]<-ST[i-1]+Tmoy[i]
      }

    #Calcul de LAI
    Tr<-(1/B)*log(1+exp(A*TI))
    LAI<-Lmax*((1/(1+exp(-A*(ST-TI))))-exp(B*(ST-(Tr))))
    LAI[LAI<0]<-0

    #Calcul de la biomasse (g/m2)
    U<-Eb*Eimax*(1-exp(-K*LAI))*PAR
    BIOMASSE<-sum(U)

   # BIOMASSE
    U <- cumsum(U)
    U
  }
#===========================================================================
#biomasse.nominal <- c(1.85,0.94,0.7,7.5,0.0065,0.00205,900)
#===========================================================================
#biomasse.input <- list(Eb=c(0.9,2.8),Eimax=c(0.9,0.99),K=c(0.6,0.8),Lmax=c(3,12),A=c(0.0035,0.01),B=c(0.0011,0.0025),TI=c(700,1100),Clim=c(1:14))
#attributes(biomasse.input)$qualitatif <- c(rep(F,7),T)
#===========================================================================

