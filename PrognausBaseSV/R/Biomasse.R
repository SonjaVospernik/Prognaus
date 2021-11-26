###########################################################################
#Biomassefunktion
###########################################################################
#'@title Biomass functions
#'
#'@description \code{Biomasse} calculates the branch and needle biomass in kg/tree
#'
#'@details
#'Calculates the branch and needle biomass. The model for spruce and pine
#'needle and branch biomass is taken from Eckmüllner (2006).
#'Models for deciduous trees are taken from
#'Gschwandtner and Schadauer (2006).
#'For full references see
#'\url{https://homepage.boku.ac.at/sonja/Lit_Prog.pdf}.
#'Coding of tree species Austrian National Forest Inventory
#'(\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}).
#'
#'@param BART Tree species,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI},
#'default=1 Norway spruce
#'@param BHD_cm dbh in cm
#'@param H_m Tree height in m
#'@param CR_Ant Crown ratio as ratio
#'@param Typ Type of biomass "Ast"=branch biomass, "Nadel"=needle biomass; For broadleaved species
#'there is no needle or leaf biomass. Default is branch biomass
#'@param Modell Type of biomass model, 1-dbh, 2-dbh, height, 3-dbh, height, ln(1-crown ratio)
#'4-dbh, height, (1-crown ratio). Default is the model based on dbh only.
#'@param Nadelv Dummy variable for needle defoliation, 0-no defoliation, 1-defoliated
#'@param Korr Bias correction for logarithmic transformation bias, TRUE=correction, FALSE=no correction
#'
#'
#'@examples
#'Biomasse(BART=1,BHD_cm=30)

Biomasse<-function(BART=1, BHD_cm, H_m=20, CR_Ant=0.5, Typ="Ast", Modell=1,
                     Nadelv=0, Korr=T){ #CR=Crown ratio

##########################################################################
#Fichte, Nadel
##########################################################################
if((BART==1 | BART==2 | BART==3) & Typ=="Nadel" & Modell==1){
    a0=-2.4055
    a1=-0.2718
    a2=1.6882
    a3=0
    a4=0
    a5=0
    m=1.140
 }

if((BART==1 | BART==2 | BART==3) & Typ=="Nadel" & Modell==2){
      a0=-1.6386
      a1=-0.2788
      a2=2.0005
      a3=-0.5944
      a4=0
      a5=0
      m=1.133
 }

if((BART==1 | BART==2 | BART==3) & Typ=="Nadel" & Modell==3){
      a0=-2.6347
      a1=-0.2708
      a2=1.9262
      a3=-0.3124
      a4=-0.3646
      a5=0
      m=1.120
 }

if((BART==1 | BART==2 | BART==3) & Typ=="Nadel" & Modell==4){
      a0=-1.7701
      a1=-0.2701
      a2=1.9710
      a3=-0.3982
      a4=0
      a5=-0.9881
      m=1.124
 }

#####################################################################################
#Ast
#####################################################################################

if((BART==1 | BART==2 | BART==3) & Typ=="Ast" & Modell==1){
      a0=-4.0458
      a1=-0.1736
      a2=2.3215
      a3=0
      a4=0
      a5=0
      m=1.096
  }

if((BART==1 | BART==2 | BART==3) & Typ=="Ast" & Modell==2){
    a0=-2.6959
    a1=-0.1859
    a2=2.8712
    a3=-1.0462
    a4=0
    a5=0
    m=1.077
}

if((BART==1 | BART==2 | BART==3) & Typ=="Ast" & Modell==3){
    a0=-3.6270
    a1=-0.1784
    a2=2.8018
    a3=-0.7826
    a4=-0.3408
    a5=0
    m=1.065
}

if((BART==1 | BART==2 | BART==3) & Typ=="Ast" & Modell==4){
    a0=-2.8258
    a1=-0.1773
    a2= 2.8420
    a3=-0.8525
    a4=0
    a5=-0.9760
    m=1.068
}

######################################################################
# Nadel, Kiefer
######################################################################
if((BART==4 | BART==5 | BART==6) & Typ=="Nadel" & Modell==1){
    a0=-2.0776
    a1=-0.3200
    a2=1.3369
    a3=0
    a4=0
    a5=0
    m=1.061
  }

if((BART==4 | BART==5 | BART==6) & Typ=="Nadel" & Modell==2){
    a0=-1.0056
    a1=-0.2591
    a2=1.9365
    a3=-0.9844
    a4=0
    a5=0
    m=1.050
}

if((BART==4 | BART==5 | BART==6) & Typ=="Nadel" & Modell==3){
    a0=-0.5480
    a1=-0.2635
    a2=2.0816
    a3=-1.4727
    a4=-0.7164
    a5=0
    m=1.048
  }

if((BART==4 | BART==5 | BART==6) & Typ=="Nadel" & Modell==4){
    a0=0
    a1=0
    a2=0
    a3=0
    a4=0
    a5=0
    m=0
  }

#######################################################################
# Ast, Kiefer
#######################################################################
if((BART==4 | BART==5 | BART==6) & Typ=="Ast" & Modell==1){
     a0=-2.1917
      a1=-0.1961
      a2=1.6665
      a3=0
      a4=0
      a5=0
      m= 1.039
  }

if((BART==4 | BART==5 | BART==6) & Typ=="Ast" & Modell==2){
    a0=-1.6788
    a1=-0.1670
    a2=1.9534
    a3=-0.4710
    a4=0
    a5=0
    m=1.037
  }

if((BART==4 | BART==5 | BART==6) & Typ=="Ast" & Modell==3){
    a0=-1.1656
    a1=-0.1718
    a2=2.1163
    a3=-1.0197
    a4=-0.8051
    a5=0
    m=1.035
  }

if((BART==4 | BART==5 | BART==6) & Typ=="Ast" & Modell==4){
    a0=0
    a1=0
    a2=0
    a3=0
    a4=0
    a5=0
    m=0
}

NH.vec<-c(1,2,3,4,5,6)

#Modell für Nadelhölzer
if(BART %in% NH.vec){
  ln.Bio=a0+a1*Nadelv+a2*log(BHD_cm)+a3*log(H_m)+a4*log(1-CR_Ant)+a5*(1-CR_Ant)
  Bio=exp(ln.Bio)

  if(BART==3){
    Bio<-Bio*0.5
  }

  if(Korr==T){#Korrektur für ln-Transformation
    Bio=Bio*m
  }#End if
}


if(BART==10){
  b0=-9.31816
  b1=3.76340
  b2=0.59888
  b3=0.15215
  lambda=1.38805
}

if(BART==11){
  b0=-7.14617
  b1=4.08950
  b2= 0.28758
  b3=-0.98244
  lambda=1.14903
}

if(BART==13){
  b0=-9.14778
  b1=3.28360
  b2=1.32930
  b3=0.83335
  lambda=1.18795
}

if(BART==12){
  b0=-8.51170
  b1=3.62069
  b2=0.58391
  b3=0.31751
  lambda=1.16257
}

if(BART==20){
  b0=-6.07105
  b1=3.16964
  b2=0.30901
  b3=-0.38339
  lambda=1.26017
}

if(BART==24 | BART==26){
  b0=-9.06122
  b1=3.92213
  b2=0.68806
  b3=-0.39490
  lambda=1.30463
}

if(BART==27){
  b0=-6.48384
  b1=2.57295
  b2=0.87237
  b3= 0.22986
  lambda=1.37975
}

LH.vec<-c(10,11,12,13,20,24,26,27)

#Modell für Laubhölzer
if(BART %in% LH.vec){
  ln.Bio<-b0+b1*log(BHD_cm)+b2*log(CR_Ant)+b3*log(H_m)
  Bio=exp(ln.Bio)


  if(Korr==T){#Korrektur für ln-Transformation
    Bio=Bio*lambda
  }#End if

  if(Typ=="Nadel"){
    Bio=0
  }
}

return(Bio)

}#End function

Biomasse(BART=1,BHD_cm=30)

Biomasse<-Vectorize(Biomasse)

###########################################################################
#Specific weight
##########################################################################
#'@title Specific weight
#'
#'@description \code{Spez.Gewicht} calculates the specific weight in kg/m³
#'
#'@details
#'Calculates the specific weight according to usage of trait in Austria ÖHU(2006)
#'
#'@param BART Tree species,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI},
#'default=1 Norway spruce
#'
#'@examples
#'Spez.Gewicht(BART=1)
#'

Spez.Gewicht<-function(BART=1){
  BaumartOewi=c(1,2,4,3,26,23,24,27,21,20,14,11,10,13,12)
  FOO=c(427,427,510,545,350,383,422,425,460,515,570,630,650,650,680)
  #FMO=c(475,475,570,625,402,445,479,500,541,585,633,741,707,755,739)
  Spez_Gewicht<-FOO[which(BaumartOewi==BART)]

}

Spez.Gewicht<-Vectorize(Spez.Gewicht)

