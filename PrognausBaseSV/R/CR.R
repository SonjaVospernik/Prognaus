#############################################################
#Crown ratio Prognaus
#############################################################
#'@title Crown ratio model Prognaus
#'
#'@description \code{CR.P} calculates the crown ratio
#'
#'@details
#'CR.P is a static crown ratio model, i.e. it estimates the crown ratio
#'from tree and site variables. Dynamic crown ratio models predict the
#'change in crown ratio. The model was developed by
#'Hasenauer and Monserud (1996). Coefficients are taken from Hasenauer (2000).
#'For full references see
#'\url{https://homepage.boku.ac.at/sonja/Lit_Prog.pdf}.
#'Coding of tree species, uses the coding of the
#'Austrian National Forest Inventory
#'(\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}).
#'
#'@param BART Tree species,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI},
#'default=1 Norway spruce
#'@param BHD_cm Dbh in cm
#'@param H_m Tree height in m
#'@param BAL Basal area of larger trees (Wykoff, 1990)
#'@param CCF Crown competition factor (Krajicek, 1961)
#'@param Seehoehe_m Elevation in m (Range: 200-2200 m)
#'@param Hangneigung_Proz Slope in percent
#'@param Exposition_Grad Aspect in degree
#'
#'@examples
#'CR.P(BART=1,BHD_cm=30,H_m=30,BAL=0,CCF=200)

CR.P<-function(BART=1, BHD_cm, H_m, BAL, CCF,
               Seehoehe_m=600, Hangneigung_Proz=50, Exposition_Grad=180){

#Berechnet das Kronenmodel mit den Koeffizienten von Hasenauer 2000
if (BART==1) {#Fichte
  a0 = -3.893748
  b1 = 0.011232
  b2 = 0.02687
  b3 = -0.6358 / 10000
  c1 = 0.010574
  c2 = 0.404436
  d1 = -0.043123
  d2 = 0
  d3 = 0
  d4 = -0.222507
  d5 = -0.040207
  d6 = 0.147378
}

if (BART==2) {#Tanne
  a0 = -2.364763
  b1 = 0.007637
  b2 = 0.022854
  b3 = -0.74049 / 10000
  c1 = 0.010184
  c2 = 0.270049
  d1 = -0.030867
  d2 = 0
  d3 = 0
  d4 = -0.555051
  d5 = -0.030725
  d6 = 0.158594
}

if (BART==3) {#Lärche
  a0 = -2.859456
  b1 = 0.009106
  b2 = 0.027547
  b3 = 0 / 10000
  c1 = 0.009133
  c2 = 0.373768
  d1 = -0.037037
  d2 = 0
  d3 = -0.251112
  d4 = 0
  d5 = 0.060256
  d6 = 0.106756
}

if (BART==4) {#Kiefer
  a0 = -1.676984
  b1 = 0.002942
  b2 = 0.026387
  b3 = 0 / 10000
  c1 = 0.013054
  c2 = 0.245749
  d1 = 0
  d2 = -0.001494
  d3 = 0
  d4 = -0.413472
  d5 = 0.079776
  d6 = 0.167692
}

if (BART==5) {#Schwarzkiefer
  a0 = -2.262052
  b1 = 0
  b2 = 0.019348
  b3 = 0 / 10000
  c1 = 0.013737
  c2 = 0.449099
  d1 = 0
  d2 = -0.012183
  d3 = 0
  d4 = 0.412069
  d5 = 0.257688
  d6 = -0.135794
}

if (BART==6) {#Zirbe
  a0 = -5.601587
  b1 = 0.018602
  b2 = 0.031416
  b3 = 0 / 10000
  c1 = 0
  c2 = 0.674139
  d1 = 0
  d2 = 0
  d3 = 0
  d4 = 0
  d5 = 0
  d6 = 0
}

if (BART==10) {#Buche
  a0 = -3.853739
  b1 = 0.007435
  b2 = 0.024684
  b3 = -1.35686 / 10000
  c1 = 0
  c2 = 0.384344
  d1 = 0.015155
  d2 = 0
  d3 = 0.144669
  d4 = 0
  d5 = 0
  d6 = 0
}

if (BART==11) {#Eiche
  a0 = -1.985395
  b1 = 0.005525
  b2 = 0.013867
  b3 = -1.10735 / 10000
  c1 = 0.006856
  c2 = 0.201014
  d1 = 0
  d2 = 0
  d3 = 0
  d4 = 0
  d5 = 0
  d6 = 0
}

if (BART==28) {#sonst. Laubholz
  a0 = -1.995385
  b1 = 0.00649
  b2 = 0.019637
  b3 = -1.2235 / 10000
  c1 = 0.003633
  c2 = 0.158546
  d1 = 0
  d2 = 0
  d3 = 0
  d4 = 0.17509
  d5 = 0.015639
  d6 = 0.074927
}


Expo_rad  <-  (Exposition_Grad / 180) * pi #Umrechnung in Radiant
if(CCF==0){
  CCF<-1
}


Crown_ratio = 1 / (1 + exp(a0 + b1 * (100 * H_m / BHD_cm) + b2 * H_m + b3 * BHD_cm ^ 2+
c1 * BAL + c2 * log(CCF) + d1 * Seehoehe_m / 100 + d2 * (Seehoehe_m / 100) ^ 2 +
d3 * Hangneigung_Proz / 100 + d4 * (Hangneigung_Proz / 100) ^ 2 +
d5 * (Hangneigung_Proz / 100) * sin(Expo_rad) + d6 * (Hangneigung_Proz / 100) * cos(Expo_rad)))

}

CR.P<-Vectorize(CR.P)
