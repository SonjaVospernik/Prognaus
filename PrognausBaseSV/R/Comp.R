#'@title Open grown tree crown diameter
#'
#'@description \code{SD.P} calculates the crown diameter of open-grown trees in m
#'
#'@details
#'The model is based on Hasenauer (1997).
#'For full references see
#'\url{https://homepage.boku.ac.at/sonja/Lit_Prog.pdf}.
#'Coding of tree species uses the coding of the
#'Austrian National Forest Inventory
#'(\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}).
#'
#'@param BART Tree species,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI},
#'default=1 Norway spruce
#'@param BHD_cm dbh in cm
#'@param MSE_ja Dummy variable, default=0, if 1 half of the model MSE is
#'added to correct for logarithmic transformation bias

SD.P<-function(BART=1, BHD_cm, MSE_ja=0){

if (BART==1) {#Fichte
  b0 = -0.3232
  b1 = 0.6441
  MSE = 0.026
}

if (BART==2) {#Tanne
  b0 = 0.092
  b1 = 0.538
  MSE = 0.03
}

if (BART==3) {#Lärche
  b0 = -0.3396
  b1 = 0.6823
  MSE = 0.035
}

if (BART==4) {#Kiefer
  b0 = -0.1797
  b1 = 0.6267
  MSE = 0.037
}

if (BART==5) {#Schwarzkiefer
  b0 = -0.157
  b1 = 0.631
  MSE = 0.019
}

if (BART==6) {#Zirbe
  b0 = -1.3154
  b1 = 0.8288
  MSE = 0.2
}

if (BART==10) {#Buche
  b0 = 0.2662
  b1 = 0.6072
  MSE = 0.058
}

if (BART==11) {#Eiche
  b0 = -0.3973
  b1 = 0.7328
  MSE = 0.038
}

if (BART==13) {#Esche
  b0 = 0.1366
  b1 = 0.6183
  MSE = 0.051
}

if (BART==14) {#Ahorn
  b0 = 0.418
  b1 = 0.5285
  MSE = 0.025
}

if (BART==20|BART==28) {#Birke
  b0 = 0.1783
  b1 = 0.5665
  MSE = 0.025
}

SD.Prog = exp(b0 + b1 * log(BHD_cm) + 0.5 * MSE * MSE_ja)
}

SD.P<-Vectorize(SD.P)

#'@title Open-grown tree crown area
#'
#'@description \code{EZ} calculates the represented open-grown tree crown area
#'in m²
#'
#'@details
#'Calculates the represented open-grown crown area from an open-grown tree
#'diameter \code{\link{SD.P}} assuming circular crown areas and multiplies
#'them by the represented stem number (e.g. in fixed area plots or
#'angle count samples)
#'@param SD_m Diameter of an open-grown tree in meter
#'@param nrep represented stem number, default=1, data is not from
#'a sample, i.e. each tree represents 1 tree per hectare
#'
EZ<-function(SD_m, nrep=1){
#Berechnet eine kreisförmige Einflusszone um den Durchmesser
  SR_m = SD_m / 2
  Einflusszonen = SR_m * SR_m * pi * nrep
}

EZ<-Vectorize(EZ)

#'@title Crown competition factor
#'
#'@description \code{CCF.P} calculates the crown competition factor
#'
#'@details
#'Calculates the crown competition factor according to Krajicek et al. (1961)
#'using the open-grown tree diameter functions developed
#'by Hasenauer (1997) \code{\link{SD.P}}.Circular crown areas are assumed
#'and multiplied by the represented stem number (e.g. in fixed area plots or
#'angle count samples)
#'@param ID Unique ID for each sample plot
#'@param BART Tree species,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI},
#'default=1 Norway spruce
#'@param BHD_cm dbh in cm
#'@param nrep represented stem number, default=1, data is not from
#'a sample, i.e. each tree represents 1 tree per hectare
#'@param MSE_ja Dummy Variable, default=0, if 1 half of the model MSE is
#'added to correct for logarithmic transformation bias in the crown diameter
#'model

##!Achtung!!!! Kein Vectorize!!! erzeugt hier Unfug!!!!!
CCF.P<-function(ID_Punkt=1, BART=1, BHD_cm, nrep=1, MSE_ja=0){

  SolD<-SD.P(BART,BHD_cm,MSE_ja)
  Crownc<-EZ(SolD,nrep)
  dat <- data.frame("Crownc"=Crownc, "ID_Punkt"=ID_Punkt)
  dat1<-aggregate(dat$Crownc, by=list(dat$ID_Punkt), FUN=sum)
  dat2<-merge(dat, dat1, by.x="ID_Punkt", by.y="Group.1", sort="FALSE")
  dat2$CCF<-dat2$x/100
  CCF<-dat2$CCF
  }

#'@title Basal area of larger trees
#'
#'@description \code{BAL.P} calculates the basal area of larger trees in m²/ha
#'
#'@details
#'Calculates the basal area of larger trees according to Wykoff 1990.
#'@param ID_Punkt Unique ID for each sample plot
#'@param ID_Baum Unique ID for each tree within a plot
#'@param BHD_cm dbh in cm
#'@param G_m2ha represented basal area in m²/ha

BAL.P<-function(ID_Punkt=1, ID_Baum, BHD_cm, G_m2ha){
  dat<-data.frame("ID_Punkt"=ID_Punkt, "ID_Baum"=ID_Baum, "BHD"=BHD_cm,
                  "Gha"=G_m2ha)
  temp<-dat
  #Nach Probefläche und BHD sortieren
  temp<-temp[order(temp$ID_Punkt, -temp$BHD),]
  temp$BAL<-ave(temp$Gha,by=list(temp$ID_Punkt),
                 FUN=function(x) c(0, cumsum(x)[1:length(x)-1]))

  dat1<-merge(dat, temp[,c("ID_Punkt", "ID_Baum", "BAL")],
              by.x=c("ID_Punkt", "ID_Baum"),
              by.y=c("ID_Punkt", "ID_Baum"), sort=FALSE)

  BAL<-dat1$BAL

}


