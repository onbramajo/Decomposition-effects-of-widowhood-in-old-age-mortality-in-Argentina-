rm(list=ls())
library(readr)
library(tidyverse)
ANSES <- read_table2("Data Viudez Argentina.txt")


head(ANSES)



# JO (Beneficios Regulares) + MOR (Moratorias) para comparar vs SP (Pensión de sobrevivencia)

ANSES$JOMOR <- ANSES$JO+ANSES$MOR
ANSES$muerteJOMOR <-ANSES$muerteJO+ANSES$muerteMOR



###Tasas Brutas de Mortalidad

ANSES$tasaJO <-ANSES$muerteJO/ANSES$JO
ANSES$tasaMOR <-ANSES$muerteMOR/ANSES$MOR
ANSES$tasaSP <-ANSES$muerteSP/ANSES$SP
ANSES$tasaJOMOR <-ANSES$muerteJOMOR/ANSES$JOMOR
ANSES$tasaTOT <-ANSES$muerteTotal/ANSES$Total

##agrego valores "low y high" para poner intervalos de confianza de 95% (1.96 desvios de la media)

ANSES$lowJO<- (1/ANSES$JO) *(ANSES$muerteJO -(sqrt(ANSES$muerteJO)*1.96)) 
ANSES$lowMOR<- (1/ANSES$MOR) *(ANSES$muerteMOR -(sqrt(ANSES$muerteMOR)*1.96))
ANSES$lowSP<- (1/ANSES$SP) *(ANSES$muerteSP -(sqrt(ANSES$muerteSP)*1.96))
ANSES$lowJOMOR<- (1/ANSES$JOMOR) *(ANSES$muerteJOMOR -(sqrt(ANSES$muerteJOMOR)*1.96))
ANSES$lowTOT<- (1/ANSES$Total) *(ANSES$muerteTotal -(sqrt(ANSES$muerteTotal)*1.96))

ANSES$hiJO<- (1/ANSES$JO) *(ANSES$muerteJO + (sqrt(ANSES$muerteJO)*1.96)) 
ANSES$hiMOR<- (1/ANSES$MOR) *(ANSES$muerteMOR + (sqrt(ANSES$muerteMOR)*1.96))
ANSES$hiSP<- (1/ANSES$SP) *(ANSES$muerteSP +(sqrt(ANSES$muerteSP)*1.96))
ANSES$hiJOMOR<- (1/ANSES$JOMOR) *(ANSES$muerteJOMOR +(sqrt(ANSES$muerteJOMOR)*1.96))
ANSES$hiTOT<- (1/ANSES$Total) *(ANSES$muerteTotal +(sqrt(ANSES$muerteTotal)*1.96))


#Tasa Bruta de mortalidad (CDR Crude Death Rate)

CDtasa <- ANSES  %>%
  group_by(Sex)  %>%
  summarize(DtasaJO=sum(muerteJO)/sum(JO)*1000,
            DtasaMOR=sum(muerteMOR)/sum(MOR)*1000,
            DtasaSP=sum(muerteSP)/sum(SP)*1000,
            DtasaJOMOR=sum(muerteJOMOR)/sum(JOMOR)*1000,
            DtasalowJO=sum(lowJO*JO)/sum(JO)*1000,
            DtasalowMOR=sum(lowMOR*MOR)/sum(MOR)*1000,
            DtasalowSP=sum(lowSP*SP)/sum(SP)*1000,
            DtasalowJOMOR=sum(lowJOMOR*JOMOR)/sum(JOMOR)*1000,
            DtasahiJO=sum(hiJO*JO)/sum(JO)*1000,
            DtasahiMOR=sum(hiMOR*MOR)/sum(MOR)*1000,
            DtasahiSP=sum(hiSP*SP)/sum(SP)*1000,
            DtasahiJOMOR=sum(hiJOMOR*JOMOR)/sum(JOMOR)*1000)
print(CDtasa)
write.table(CDtasa, "Distribuciontasasbrutas.txt",sep="\t")

##Tasa de mortalidad por grupos grandes de edad

ANSES$AgeGroups <-0

ANSES$AgeGroups [ANSES$Age<80]<-"65-79"
ANSES$AgeGroups [ANSES$Age>80]<-"80-99"



DtasaAGE <- ANSES  %>%
    group_by(Sex,AgeGroups)  %>%
  summarize(DtasaJO=sum(muerteJO)/sum(JO)*1000,
            DtasaMOR=sum(muerteMOR)/sum(MOR)*1000,
            DtasaSP=sum(muerteSP)/sum(SP)*1000,
            DtasaJOMOR=sum(muerteJOMOR)/sum(JOMOR)*1000,
            DtasalowJO=sum(lowJO*JO)/sum(JO)*1000,
            DtasalowMOR=sum(lowMOR*MOR)/sum(MOR)*1000,
            DtasalowSP=sum(lowSP*SP)/sum(SP)*1000,
            DtasalowJOMOR=sum(lowJOMOR*JOMOR)/sum(JOMOR)*1000,
            DtasahiJO=sum(hiJO*JO)/sum(JO)*1000,
            DtasahiMOR=sum(hiMOR*MOR)/sum(MOR)*1000,
            DtasahiSP=sum(hiSP*SP)/sum(SP)*1000,
            DtasahiJOMOR=sum(hiJOMOR*JOMOR)/sum(JOMOR)*1000)

print(DtasaAGE)

write.table(DtasaAGE, "Distribuciontasasporedad.txt",sep="\t")

###Tasas estandarizadas 
#Obtain estructura estándar (total de la población) 

ANSES$PopTotal <- sum(ANSES$Total[15:21])
ANSES$Structure<-ANSES$Total/ANSES$PopTotal
ANSES$Structure[1:7] <- ANSES$Structure[15:21]
ANSES$Structure[8:14]<- ANSES$Structure[15:21]

ANSES$StandJO<-ANSES$tasaJO *ANSES$Structure 
ANSES$StandMOR<-ANSES$tasaMOR *ANSES$Structure 
ANSES$StandSP<-ANSES$tasaSP *ANSES$Structure 
ANSES$StandJOMOR<-ANSES$tasaJOMOR *ANSES$Structure 

ANSES$StandlowJO<-ANSES$lowJO *ANSES$Structure 
ANSES$StandlowMOR<-ANSES$lowMOR *ANSES$Structure 
ANSES$StandlowSP<-ANSES$lowSP *ANSES$Structure 
ANSES$StandlowJOMOR<-ANSES$lowJOMOR *ANSES$Structure 

ANSES$StandhiJO<-ANSES$hiJO *ANSES$Structure 
ANSES$StandhiMOR<-ANSES$hiMOR *ANSES$Structure 
ANSES$StandhiSP<-ANSES$hiSP *ANSES$Structure 
ANSES$StandhiJOMOR<-ANSES$hiJOMOR *ANSES$Structure 

Stasa <- ANSES  %>%
  group_by(Sex)  %>%
  summarize(StasaJO=sum(StandJO)*1000,
            StasaMOR=sum(StandMOR)*1000,
            StasaSP=sum(StandSP)*1000,
            StasaJOMOR=sum(StandJOMOR)*1000,
            StasalowJO=sum(StandlowJO)*1000,
            StasalowMOR=sum(StandlowMOR)*1000,
            StasalowSP=sum(StandlowSP)*1000,
            StasalowJOMOR=sum(StandlowJOMOR)*1000,
            StasahiJO=sum(StandhiJO)*1000,
            StasahiMOR=sum(StandhiMOR)*1000,
            StasahiSP=sum(StandhiSP)*1000,
            StasahiJOMOR=sum(StandhiJOMOR)*1000
            )

Stasa$SPJO<-Stasa$StasaSP/Stasa$StasaJO
Stasa$SPMOR<-Stasa$StasaSP/Stasa$StasaMOR
Stasa$SPJOMOR<-Stasa$StasaSP/Stasa$StasaJOMOR
Stasa$lowSPJO<-Stasa$StasalowSP/Stasa$StasalowJO
Stasa$lowSPMOR<-Stasa$StasalowSP/Stasa$StasalowMOR
Stasa$lowSPJOMOR<-Stasa$StasalowSP/Stasa$StasalowJOMOR
Stasa$hiSPJO<-Stasa$StasahiSP/Stasa$StasahiJO
Stasa$hiSPMOR<-Stasa$StasahiSP/Stasa$StasahiMOR
Stasa$hiSPJOMOR<-Stasa$StasahiSP/Stasa$StasahiJOMOR

head(Stasa)

write.table(Stasa, "Distribuciontasasestandarizadas.txt",sep="\t")



## Descomposición de Kitagawa


# Separo el dataset por sexo
Males <-ANSES %>%
  filter(Sex=="Males")
Females <-ANSES %>%
  filter(Sex=="Females")

CDRMales <- CDtasa %>% filter(Sex=="Males")
mean(CDRMales$DtasaJO)

#tasas brutas de mortalidad hombres

CDRSPM <- sum(Males$muerteSP)/sum(Males$SP)
CDRJOM <- sum(Males$muerteJO)/sum(Males$JO)
CDRMORM <- sum(Males$muerteMOR)/sum(Males$MOR)
CDRJOMORM <- sum(Males$muerteJOMOR)/sum(Males$JOMOR)


CDRSPM *1000
CDRJOM *1000
CDRMORM *1000
CDRJOMORM * 1000

## y mujeres

CDRSPF <- sum(Females$muerteSP)/sum(Females$SP)
CDRJOF <- sum(Females$muerteJO)/sum(Females$JO)
CDRMORF <- sum(Females$muerteMOR)/sum(Females$MOR)
CDRJOMORF <- sum(Females$muerteJOMOR)/sum(Females$JOMOR)

CDRSPM *1000
CDRJOM *1000
CDRMORM *1000
CDRJOMORM *1000

#Cambio en la CDR o Tasa Bruta entre SP y los grupos de varones (123) y Mujeres (456)

Dif1 <- (CDRSPM - CDRJOM)*1000
Dif1
Dif2 <- (CDRSPM - CDRMORM)*1000
Dif2
Dif3 <- (CDRSPM - CDRJOMORM)*1000
Dif3

Dif4 <- (CDRSPF - CDRJOF)*1000
Dif4
Dif5 <- (CDRSPF - CDRMORF)*1000
Dif5
Dif6 <- (CDRSPF - CDRJOMORF)*1000
Dif6

###Decomposición: Diferencia de tasas= RE rate effect + CE composition effect

RE1 <- sum(0.5*(Males$SP/sum(Males$SP) + Males$JO/sum(Males$JO))*(Males$tasaSP-Males$tasaJO))
RE1 *1000

CE1 <- sum(0.5*(Males$tasaSP+Males$tasaJO)*(Males$SP/sum(Males$SP)-Males$JO/sum(Males$JO)))
CE1 *1000

RE2 <- sum(0.5*(Males$SP/sum(Males$SP) + Males$MOR/sum(Males$MOR))*(Males$tasaSP-Males$tasaMOR))
RE2*1000

CE2 <- sum(0.5*(Males$tasaSP+Males$tasaMOR)*(Males$SP/sum(Males$SP)-Males$MOR/sum(Males$MOR)))
CE2*1000


RE3 <- sum(0.5*(Males$SP/sum(Males$SP) + Males$JOMOR/sum(Males$JOMOR))*(Males$tasaSP-Males$tasaJOMOR))
RE3*1000

CE3 <- sum(0.5*(Males$tasaSP+Males$tasaJOMOR)*(Males$SP/sum(Males$SP)-Males$JOMOR/sum(Males$JOMOR)))
CE3*1000

RE4 <- sum(0.5*(Females$SP/sum(Females$SP) + Females$JO/sum(Females$JO))*(Females$tasaSP-Females$tasaJO))
RE4*1000

CE4 <- sum(0.5*(Females$tasaSP+Females$tasaJO)*(Females$SP/sum(Females$SP)-Females$JO/sum(Females$JO)))
CE4*1000

RE5 <- sum(0.5*(Females$SP/sum(Females$SP) + Females$MOR/sum(Females$MOR))*(Females$tasaSP-Females$tasaMOR))
RE5*1000

CE5 <- sum(0.5*(Females$tasaSP+Females$tasaMOR)*(Females$SP/sum(Females$SP)-Females$MOR/sum(Females$MOR)))
CE5*1000


RE6 <- sum(0.5*(Females$SP/sum(Females$SP) + Females$JOMOR/sum(Females$JOMOR))*(Females$tasaSP-Females$tasaJOMOR))
RE6*1000

CE6 <- sum(0.5*(Females$tasaSP+Females$tasaJOMOR)*(Females$SP/sum(Females$SP)-Females$JOMOR/sum(Females$JOMOR)))
CE6*1000


#Comparo los resultados de la descomposición con la diferencia original, debiera ser 0 o parecido para comprobar 
decres1<- (RE1*1000 + CE1*1000)

decres1-Dif1

decres2<- (RE2*1000 + CE2*1000)

decres2-Dif2

decres3<- (RE3*1000 + CE3*1000)

decres3-Dif3

decres4<- (RE4*1000 + CE4*1000)

decres4-Dif4

decres5<- (RE5*1000 + CE5*1000)

decres5-Dif5

decres6<- (RE6*1000 + CE6*1000)

decres6-Dif6

###Tablas de Vida: Programo las funciones

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
  
  #Lx o Años-Persona
  
nLx[length(n)] <- lx[length(n)] * nax[length(n)]
  
 # Tx
  
Tx = rev(cumsum(rev(nLx)))
  
# Esperanza de vida restante a la edad x 
ex <- Tx/lx

lifetable <- data.frame(Age=Age,n=round(n,0),nmx=round(nmx,5),nqx=round(nqx,5),
                        npx=round(npx,5),nax=round(nax,2),
                        ndx=round(ndx,0),lx=round(lx,0),Lx=round(nLx,0),
                        Tx=round(Tx,0),ex=round(ex,2))
return(lifetable)
}

###Atributos de la tabla de vida

Males$Age<-c(seq(65,95,5))
Males$nax<-2.5
Females$Age<-c(seq(65,95,5))
Females$nax<-2.5


#Tablas por sexo y prestación para variante media, lo and high
#Hombres

TableMJO <- lifetable(nmx=Males$tasaJO,Age=Males$Age,Males$nax)
TableMJO

TablelowMJO <- lifetable(nmx=Males$lowJO,Age=Males$Age,Males$nax)
TablelowMJO

TablehiMJO <- lifetable(nmx=Males$hiJO,Age=Males$Age,Males$nax)
TablehiMJO

TableMMOR <- lifetable(nmx=Males$tasaMOR,Age=Males$Age,Males$nax)
TableMMOR

TablelowMMOR <- lifetable(nmx=Males$lowMOR,Age=Males$Age,Males$nax)
TablelowMMOR

TablehiMMOR <- lifetable(nmx=Males$hiMOR,Age=Males$Age,Males$nax)
TablehiMMOR

TableMSP <- lifetable(nmx=Males$tasaSP,Age=Males$Age,Males$nax)
TableMSP

TablelowMSP <- lifetable(nmx=Males$lowSP,Age=Males$Age,Males$nax)
TablelowMSP

TablehiMSP <- lifetable(nmx=Males$hiSP,Age=Males$Age,Males$nax)
TablehiMSP

TableMJOMOR<- lifetable(nmx=Males$tasaJOMOR,Age=Males$Age,Males$nax)
TableMJOMOR

TablelowMJOMOR <- lifetable(nmx=Males$lowJOMOR,Age=Males$Age,Males$nax)
TablelowMJOMOR

TablehiMJOMOR <- lifetable(nmx=Males$hiJOMOR,Age=Males$Age,Males$nax)
TablehiMJOMOR

##Mujeres

TableFJO <- lifetable(nmx=Females$tasaJO,Age=Females$Age,Females$nax)
TableFJO


TablelowFJO <- lifetable(nmx=Females$lowJO,Age=Females$Age,Females$nax)
TablelowFJO

TablehiFJO <- lifetable(nmx=Females$hiJO,Age=Females$Age,Females$nax)
TablehiFJO

TableFMOR <- lifetable(nmx=Females$tasaMOR,Age=Females$Age,Females$nax)
TableFMOR

TablelowFMOR <- lifetable(nmx=Females$lowMOR,Age=Females$Age,Females$nax)
TablelowFMOR

TablehiFMOR <- lifetable(nmx=Females$hiMOR,Age=Females$Age,Females$nax)
TablehiFMOR

TableFSP <- lifetable(nmx=Females$tasaSP,Age=Females$Age,Females$nax)
TableFSP


TablelowFSP <- lifetable(nmx=Females$lowSP,Age=Females$Age,Females$nax)
TablelowFSP

TablehiFSP <- lifetable(nmx=Females$hiSP,Age=Females$Age,Females$nax)
TablehiFSP


TableFJOMOR <- lifetable(nmx=Females$tasaJOMOR,Age=Females$Age,Females$nax)
TableFJOMOR


TablelowFJOMOR <- lifetable(nmx=Females$lowJOMOR,Age=Females$Age,Females$nax)
TablelowFJOMOR

TablehiFJOMOR <- lifetable(nmx=Females$hiJOMOR,Age=Females$Age,Females$nax)
TablehiFJOMOR



#Agrupo las ex en una sola tabla
Agegroups<-c(TableMJO$Age)
MeanMJO<-c(TableMJO$ex)
LowMJO<-c(TablelowMJO$ex)
HiMJO<-c(TablehiMJO$ex)
MeanMMOR<-c(TableMMOR$ex)
LowMMOR<-c(TablelowMMOR$ex)
HiMMOR<-c(TablehiMMOR$ex)
MeanMSP<-c(TableMSP$ex)
LowMSP<-c(TablelowMSP$ex)
HiMSP<-c(TablehiMSP$ex)
MeanMJOMOR<-c(TableMJOMOR$ex)
LowMJOMOR<-c(TablelowMJOMOR$ex)
HiMJOMOR<-c(TablehiMJOMOR$ex)
MeanFJO<-c(TableFJO$ex)
LowFJO<-c(TablelowFJO$ex)
HiFJO<-c(TablehiFJO$ex)
MeanFMOR<-c(TableFMOR$ex)
LowFMOR<-c(TablelowFMOR$ex)
HiFMOR<-c(TablehiFMOR$ex)
MeanFSP<-c(TableFSP$ex)
LowFSP<-c(TablelowFSP$ex)
HiFSP<-c(TablehiFSP$ex)
MeanFJOMOR<-c(TableFJOMOR$ex)
LowFJOMOR<-c(TablelowFJOMOR$ex)
HiFJOMOR<-c(TablehiFJOMOR$ex)

Alltables2<-data.frame(Agegroups,MeanMJO,LowMJO,HiMJO,
                       MeanMMOR,LowMMOR,HiMMOR,
                       MeanMSP,LowMSP,HiMSP,
                       MeanMJOMOR,LowMJOMOR,HiMJOMOR,
                       MeanFJO,LowFJO,HiFJO,
                       MeanFMOR,LowFMOR,HiFMOR,
                       MeanFSP,LowFSP,HiFSP,
                       MeanFJOMOR,LowFJOMOR,HiFJOMOR)

write.table(Alltables2,"Todaslastablas.txt",sep="\t")