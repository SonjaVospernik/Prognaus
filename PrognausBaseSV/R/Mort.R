################################################################
#Mortalität Prognaus
################################################################
#'@title Mortality Model Prognaus
#'
#'@description \code{Mort.P} calculates mortality rates
#'
#'@details
#'Mort.P calculates the Mortality rates of a logistic model.
#'The model was developed by Monserud and Sterba (1999).
#'Coefficients are taken from Hasenauer (2000).
#'For full references see
#'\url{https://homepage.boku.ac.at/sonja/Lit_Prog.pdf}.
#'Coding of tree species, uses the coding of the
#'Austrian National Forest Inventory
#'(\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}).
#'
#'@param BART Tree species,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI},
#'default=1 Norway spruce
#'@param BHD_cm Dbh in cm#
#'@param BAL Basal area of larger trees (Wykoff, 1990)
#'@param CR Crown ratio as ratio
#'
#'
#'@examples
#'Mort.P(BART=1,BHD_cm=30,BAL=0,CR_Ant=0.5)
#'
#'#Deterministic
#'nrep2012<-500
#'nrep2015<-nrep2012*(1-Mort.P(BART=1,BHD_cm=30,BAL=0,CR_Ant=0.5,PL=3))
#'
#'#Stochastic
#'ZZ<-runif(n=1,min=0,max=1)
#'ifelse(Mort.P(BART=1,BHD_cm=30,BAL=0,CR_Ant=0.5)>ZZ,1,0)


Mort.P<-function(BART=1, BHD_cm, BAL=0, CR_Ant=0.5, PL=5){

if (BART==1) { #Fichte
  a0 = 2.5638
  b1 = -10.9711
  b2 = 3.184
  b3 = -0.0265
  b4 = 0.0425
  b5 = -0.00088
}
if (BART==2){#Tanne
  a0 = 4.5781
  b1 = -11.1766
  b2 = 0
  b3 = -0.0233
  b4 = 0.0425
  b5 = -0.00088
 }
if (BART==3){#Lärche
  a0 = 5.7261
  b1 = -11.8815
  b2 = 0
  b3 = -0.0484
  b4 = 0.0425
  b5 = -0.00088
}
if (BART==4){#Kiefer
  a0 = 5.1795
  b1 = -17.2146
  b2 = 0
  b3 = -0.0377
  b4 = 0.0425
  b5 = -0.00088
}
if (BART==10){#Buche
  a0 = 3.4916
  b1 = -14.7423
  b2 = 2.8851
  b3 = -0.0114
  b4 = 0.0425
  b5 = -0.00088
}
if (BART==11){#Eiche
  a0 = 4.4508
  b1 = -12.0041
  b2 = 0
  b3 = 0
  b4 = 0.0425
  b5 = -0.00088
}
if (BART==28){#sonst. Laubholz
  a0 = 4.0697
  b1 = -8.5927
  b2 = 0
  b3 = -0.0278
  b4 = 0.0425
  b5 = -0.00088
}


Logit_Mort = a0 + b1 * (1 / BHD_cm) + b2 * CR_Ant + b3 * BAL +
b4 * BHD_cm + b5 * BHD_cm ^ 2
Mort_Prognaus = 1 / (1 + exp(Logit_Mort))
Mort_Prognaus = 1 - (1 - Mort_Prognaus) ^ (PL / 5)
}

Mort.P<-Vectorize(Mort.P)


####################################################################################
#Maximale Grundfläche
####################################################################################
#'@title Maximum basal area functions
#'
#'@description \code{GMax} calculates the maximum basal area according to the Reineke-rule
#'
#'@details
#'GMax calculates the maximum basal area with 0.95 quantile coefficients according to Vospernik (2015)
#'#'For full references see
#'\url{https://homepage.boku.ac.at/sonja/Lit_Prog.pdf}.
#'
#'@param BART Tree species,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI},
#'default=1 Norway spruce
#'@param dg_cm Quadratic mean diameter in cm
#'
#'@examples
#'GMax(BART=1,dg_cm=30)

#Berechnen der maximalen Grundfläche nach Reineke
GMax<-function(BART=1, dg_cm){
  if (BART==1){#Fichte
    c0=12.85988764
    c1=-1.752808989
  }

  if (BART==2){#Tanne
    c0=12.414862385
    c1=-1.633027523
  }

  if (BART==3){#Lärche
    c0=13.004193548
    c1=-1.935483871
  }

  if (BART==4){#Kiefer
    c0=12.7675
    c1=-1.75
  }

  if (BART==5){#Schwarzkiefer
    c0=13.132467518
    c1=-1.823215242
  }

  if (BART==6){#Zirbe
    c0=11.619999989
    c1=-1.499999997
  }

  if (BART==10){#Buche
    c0=	13.252352941
    c1=-1.941176471
  }

  if (BART==11){#Eiche
    c0=12.616
    c1=-1.8
  }

  if (BART==12){#Hainbuche
    c0=13.329999999
    c1=-2
  }

  if (BART==13){#Esche
    c0=13.223333333
    c1=-1.939393939
  }

  g=pi/4*(dg_cm/100)^2
  N=exp(c0+c1*log(dg_cm))
  G=g*N
}

GMax<-Vectorize(GMax)


