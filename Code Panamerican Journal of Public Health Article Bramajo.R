library(readr)
library(tidyverse)
ANSES <- read_table2("Casos PAHO Bramajo.txt")

head(ANSES)

###Tasas Estandarizadas de Mortalidad/SMR 

ANSES$RateJO <-ANSES$MuertesJO/ANSES$JO
ANSES$RateMOR <-ANSES$MuertesMOR/ANSES$MOR
ANSES$RatePS <-ANSES$MuertesPS/ANSES$PS
ANSES$RateTOT <-ANSES$MuertesTotal/ANSES$Total

ANSES$StandJO<-ANSES$RateJO *ANSES$Estructura 
ANSES$StandMOR<-ANSES$RateMOR *ANSES$Estructura 
ANSES$StandPS<-ANSES$RatePS *ANSES$Estructura 
ANSES$StandTOT<-ANSES$RateTOT *ANSES$Estructura 


SRATE <- ANSES  %>%
  group_by(Sexo)  %>%
  summarize(SRateJO=sum(StandJO)*1000,
            SRateMOR=sum(StandMOR)*1000,
            SRatePS=sum(StandPS)*1000,
            SRateTOT=sum(StandTOT)*1000)

SRATE$PSJO<-SRATE$SRatePS/SRATE$SRateJO
SRATE$PSMOR<-SRATE$SRatePS/SRATE$SRateMOR
SRATE$PSTOT<-SRATE$SRatePS/SRATE$SRateTOT

head(SRATE)

##Descomposición de Kitagawa/ Kitagawa decomposition


# Separo po sexo Hombres y Mujeres
Hombres <-ANSES %>%
  filter(Sexo=="Hombres")
Mujeres <-ANSES %>%
  filter(Sexo=="Mujeres")

Hombres$Resto <- Hombres$JO+Hombres$MOR
Hombres$MuertesResto <-Hombres$MuertesJO+Hombres$MuertesMOR
Hombres$RateResto <- Hombres$MuertesResto/Hombres$Resto


Mujeres$Resto <- Mujeres$JO+Mujeres$MOR
Mujeres$MuertesResto <-Mujeres$MuertesJO+Mujeres$MuertesMOR
Mujeres$RateResto <- Mujeres$MuertesResto/Mujeres$Resto

#CDR/TBM Hombres/Males
CDRPSH <- sum(Hombres$MuertesPS)/sum(Hombres$PS)
CDRJOH <- sum(Hombres$MuertesJO)/sum(Hombres$JO)
CDRMORH <- sum(Hombres$MuertesMOR)/sum(Hombres$MOR)
CDRRESH <- sum(Hombres$MuertesResto)/sum(Hombres$Resto)

CDRPSH *1000
CDRJOH *1000
CDRMORH *1000
CDRRESH * 1000
##CDR/TBM Mujeres/Females

CDRPSM <- sum(Mujeres$MuertesPS)/sum(Mujeres$PS)
CDRJOM <- sum(Mujeres$MuertesJO)/sum(Mujeres$JO)
CDRMORM <- sum(Mujeres$MuertesMOR)/sum(Mujeres$MOR)
CDRRESM <- sum(Mujeres$MuertesResto)/sum(Mujeres$Resto)

CDRPSM *1000
CDRJOM *1000
CDRMORM *1000
CDRRESM *1000

#crude death expressed by 1000 
#Change in CDR entre PS y otros grupos  Hombres (1,2,3) y Mujeres (4,5,6) 
Dif1 <- (CDRPSH - CDRJOH)*1000
Dif1
Dif2 <- (CDRPSH - CDRMORH)*1000
Dif2
Dif3 <- (CDRPSH - CDRRESH)*1000
Dif3

Dif4 <- (CDRPSM - CDRJOM)*1000
Dif4
Dif5 <- (CDRPSM - CDRMORM)*1000
Dif5
Dif6 <- (CDRPSM - CDRRESM)*1000
Dif6

###Descomposiciones: RC efecto tasa - CC efecto composición (edad)

RC1 <- sum(0.5*(Hombres$PS/sum(Hombres$PS) + Hombres$JO/sum(Hombres$JO))*(Hombres$RatePS-Hombres$RateJO))
RC1 *1000

CC1 <- sum(0.5*(Hombres$RatePS+Hombres$RateJO)*(Hombres$PS/sum(Hombres$PS)-Hombres$JO/sum(Hombres$JO)))
CC1 *1000

RC2 <- sum(0.5*(Hombres$PS/sum(Hombres$PS) + Hombres$MOR/sum(Hombres$MOR))*(Hombres$RatePS-Hombres$RateMOR))
RC2*1000

CC2 <- sum(0.5*(Hombres$RatePS+Hombres$RateMOR)*(Hombres$PS/sum(Hombres$PS)-Hombres$MOR/sum(Hombres$MOR)))
CC2*1000


RC3 <- sum(0.5*(Hombres$PS/sum(Hombres$PS) + Hombres$Resto/sum(Hombres$Resto))*(Hombres$RatePS-Hombres$RateResto))
RC3*1000

CC3 <- sum(0.5*(Hombres$RatePS+Hombres$RateResto)*(Hombres$PS/sum(Hombres$PS)-Hombres$Resto/sum(Hombres$Resto)))
CC3*1000

RC4 <- sum(0.5*(Mujeres$PS/sum(Mujeres$PS) + Mujeres$JO/sum(Mujeres$JO))*(Mujeres$RatePS-Mujeres$RateJO))
RC4*1000

CC4 <- sum(0.5*(Mujeres$RatePS+Mujeres$RateJO)*(Mujeres$PS/sum(Mujeres$PS)-Mujeres$JO/sum(Mujeres$JO)))
CC4*1000

RC5 <- sum(0.5*(Mujeres$PS/sum(Mujeres$PS) + Mujeres$MOR/sum(Mujeres$MOR))*(Mujeres$RatePS-Mujeres$RateMOR))
RC5*1000

CC5 <- sum(0.5*(Mujeres$RatePS+Mujeres$RateMOR)*(Mujeres$PS/sum(Mujeres$PS)-Mujeres$MOR/sum(Mujeres$MOR)))
CC5*1000


RC6 <- sum(0.5*(Mujeres$PS/sum(Mujeres$PS) + Mujeres$Resto/sum(Mujeres$Resto))*(Mujeres$RatePS-Mujeres$RateResto))
RC6*1000

CC6 <- sum(0.5*(Mujeres$RatePS+Mujeres$RateResto)*(Mujeres$PS/sum(Mujeres$PS)-Mujeres$Resto/sum(Mujeres$Resto)))
CC6*1000


#Compare the decomposition results with the original difference, should be 0 
decres1<- (RC1*1000 + CC1*1000)

decres1-Dif1

decres2<- (RC2*1000 + CC2*1000)

decres2-Dif2

decres3<- (RC3*1000 + CC3*1000)

decres3-Dif3

decres4<- (RC4*1000 + CC4*1000)

decres4-Dif4

decres5<- (RC5*1000 + CC5*1000)

decres5-Dif5

decres6<- (RC6*1000 + CC6*1000)

decres6-Dif6

##Relative Risk Increase

(CDRJOH+RC1)/CDRJOH

(CDRMORH+RC2)/CDRMORH

(CDRRESH+RC3)/CDRRESH

(CDRJOM+RC4)/CDRJOM

(CDRMORM+RC5)/CDRMORM

(CDRRESM+RC6)/CDRRESM

###Tablas de Mortalidad/Lifetables

lifetable <- function(nmx,Age,nax){
  n <- c(diff(Age),nax[length(nax)])
  
  
  nqx <- n*nmx / (1+(n-nax)*nmx)
  npx <- 1 - nqx
  lx <- rep(NA,length(nqx))
  lx[1] <- 100000
  for(i in 1:length(n)){
    lx[i+1] <- lx[i]*npx[i]
  }
  
  lx <- lx[1:length(n)]
  ndx <- c(abs(diff(lx)),lx[length(n)])
  nLx <- rep(NA,length(n))
  for(i in 1:(length(n)-1)){
    nLx[i] <- lx[i+1]*n[i] + ndx[i]*nax[i]
  }
  
  #Person Years 
  
nLx[length(n)] <- lx[length(n)] * nax[length(n)]
  
 # Tx
  
Tx = rev(cumsum(rev(nLx)))
  
# remaining life expectancy
ex <- Tx/lx

lifetable <- data.frame(Age=Age,n=round(n,0),nmx=round(nmx,5),nqx=round(nqx,5),
                        npx=round(npx,5),nax=round(nax,2),
                        ndx=round(ndx,0),lx=round(lx,0),Lx=round(nLx,0),
                        Tx=round(Tx,0),ex=round(ex,2))
return(lifetable)
}

###Setting lifetable/Preparando la tabla de vida

Hombres$Age<-c(seq(65,95,5))
Hombres$nax<-2.5
Mujeres$Age<-c(seq(65,95,5))
Mujeres$nax<-2.5

TablaHJO <- lifetable(nmx=Hombres$RateJO,Age=Hombres$Age,Hombres$nax)
TablaHJO

TablaHMOR <- lifetable(nmx=Hombres$RateMOR,Age=Hombres$Age,Hombres$nax)
TablaHMOR

TablaHPS <- lifetable(nmx=Hombres$RatePS,Age=Hombres$Age,Hombres$nax)
TablaHPS


TablaHTOT <- lifetable(nmx=Hombres$RateTOT,Age=Hombres$Age,Hombres$nax)
TablaHTOT


TablaMJO <- lifetable(nmx=Mujeres$RateJO,Age=Mujeres$Age,Mujeres$nax)
TablaMJO

TablaMMOR <- lifetable(nmx=Mujeres$RateMOR,Age=Mujeres$Age,Mujeres$nax)
TablaMMOR

TablaMPS <- lifetable(nmx=Mujeres$RatePS,Age=Mujeres$Age,Mujeres$nax)
TablaMPS


TablaMTOT <- lifetable(nmx=Mujeres$RateTOT,Age=Mujeres$Age,Mujeres$nax)
TablaMTOT

###Lifespan Inequality
stepHJO <- rep(NA,dim(TablaHJO)[1])
for (i in 1:(dim(TablaHJO)[1]-1)){
  stepHJO[i] <- TablaHJO$ndx[i]*(TablaHJO$ex[i]+(TablaHJO$nax[i]/TablaHJO$n[i]*(TablaHJO$ex[i+1]-TablaHJO$ex[i])))
}
stepHJO[dim(TablaHJO)[1]] <- TablaHJO$ndx[dim(TablaHJO)[1]]*TablaHJO$ex[dim(TablaHJO)[1]]
edagHJO <- sum(stepHJO)/TablaHJO$lx[1]
edagHJO


stepHMOR <- rep(NA,dim(TablaHMOR)[1])
for (i in 1:(dim(TablaHMOR)[1]-1)){
  stepHMOR[i] <- TablaHMOR$ndx[i]*(TablaHMOR$ex[i]+(TablaHMOR$nax[i]/TablaHMOR$n[i]*(TablaHMOR$ex[i+1]-TablaHMOR$ex[i])))
}
stepHMOR[dim(TablaHMOR)[1]] <- TablaHMOR$ndx[dim(TablaHJO)[1]]*TablaHMOR$ex[dim(TablaHMOR)[1]]
edagHMOR <- sum(stepHMOR)/TablaHMOR$lx[1]
edagHMOR


stepHPS <- rep(NA,dim(TablaHPS)[1])
for (i in 1:(dim(TablaHPS)[1]-1)){
  stepHPS[i] <- TablaHPS$ndx[i]*(TablaHPS$ex[i]+(TablaHPS$nax[i]/TablaHPS$n[i]*(TablaHPS$ex[i+1]-TablaHPS$ex[i])))
}
stepHPS[dim(TablaHPS)[1]] <- TablaHJO$ndx[dim(TablaHJO)[1]]*TablaHJO$ex[dim(TablaHJO)[1]]
edagHPS <- sum(stepHPS)/TablaHPS$lx[1]
edagHPS


stepHTOT <- rep(NA,dim(TablaHTOT)[1])
for (i in 1:(dim(TablaHTOT)[1]-1)){
  stepHTOT[i] <- TablaHTOT$ndx[i]*(TablaHTOT$ex[i]+(TablaHTOT$nax[i]/TablaHTOT$n[i]*(TablaHTOT$ex[i+1]-TablaHTOT$ex[i])))
}
stepHTOT[dim(TablaHTOT)[1]] <- TablaHTOT$ndx[dim(TablaHTOT)[1]]*TablaHTOT$ex[dim(TablaHTOT)[1]]
edagHTOT <- sum(stepHTOT)/TablaHTOT$lx[1]
edagHTOT


stepMJO <- rep(NA,dim(TablaMJO)[1])
for (i in 1:(dim(TablaMJO)[1]-1)){
  stepMJO[i] <- TablaMJO$ndx[i]*(TablaMJO$ex[i]+(TablaMJO$nax[i]/TablaMJO$n[i]*(TablaMJO$ex[i+1]-TablaMJO$ex[i])))
}
stepMJO[dim(TablaMJO)[1]] <- TablaMJO$ndx[dim(TablaMJO)[1]]*TablaMJO$ex[dim(TablaMJO)[1]]
edagMJO <- sum(stepMJO)/TablaMJO$lx[1]
edagMJO


stepMMOR <- rep(NA,dim(TablaMMOR)[1])
for (i in 1:(dim(TablaMMOR)[1]-1)){
  stepMMOR[i] <- TablaMMOR$ndx[i]*(TablaMMOR$ex[i]+(TablaMMOR$nax[i]/TablaMMOR$n[i]*(TablaMMOR$ex[i+1]-TablaMMOR$ex[i])))
}
stepMMOR[dim(TablaMMOR)[1]] <- TablaMMOR$ndx[dim(TablaMMOR)[1]]*TablaMMOR$ex[dim(TablaMMOR)[1]]
edagMMOR <- sum(stepMMOR)/TablaMMOR$lx[1]
edagMMOR


stepMPS <- rep(NA,dim(TablaMPS)[1])
for (i in 1:(dim(TablaMPS)[1]-1)){
  stepMPS[i] <- TablaMPS$ndx[i]*(TablaMPS$ex[i]+(TablaMPS$nax[i]/TablaMPS$n[i]*(TablaMPS$ex[i+1]-TablaMPS$ex[i])))
}
stepMPS[dim(TablaMPS)[1]] <- TablaMPS$ndx[dim(TablaMPS)[1]]*TablaMPS$ex[dim(TablaMPS)[1]]
edagMPS <- sum(stepMPS)/TablaMPS$lx[1]
edagMPS


stepMTOT <- rep(NA,dim(TablaMTOT)[1])
for (i in 1:(dim(TablaMTOT)[1]-1)){
  stepMTOT[i] <- TablaMTOT$ndx[i]*(TablaMTOT$ex[i]+(TablaMTOT$nax[i]/TablaMTOT$n[i]*(TablaMTOT$ex[i+1]-TablaMTOT$ex[i])))
}
stepMTOT[dim(TablaMTOT)[1]] <- TablaMTOT$ndx[dim(TablaMTOT)[1]]*TablaMTOT$ex[dim(TablaMTOT)[1]]
edagMTOT <- sum(stepMTOT)/TablaMTOT$lx[1]
edagMTOT

