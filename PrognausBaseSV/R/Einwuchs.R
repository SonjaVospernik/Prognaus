###########################################################################
#Jugend II
###########################################################################
#'@title Regeneration II model Prognaus
#'
#'@description \code{JugendII.P} calculates the occurrence of regeneration II on a plot (TRUE/FALSE)
#'
#'@details
#'Calculates if regeneration II occurs on the plot (presence/absence). The model is part ot the modelling system
#'for calculating ingrowth on plots developed by Ledermann (2002).
#'For full references see
#'\url{https://homepage.boku.ac.at/sonja/Lit_Prog.pdf}.
#'Coding of operation type, growth district and soil type uses the coding of the
#'Austrian National Forest Inventory
#'(\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}).
#'
#'
#'@param dg_cm Quadratic mean diameter in cm
#'@param sum_G Basal area m²/ha
#'@param CCF Crown competition factor (Krajicek, 1961)
#'@param Seehoehe_m Elevation in m (Range: 200-2200 m)
#'@param Hangneigung_Proz Slope in percent
#'@param J1  Presence(1) / absence(0) of regeneration I (height: 0m-1.3m)
#'@param J2 Presence(1) / absence(0) of regeneration II (height:1.3m-dbh=10.4cm)
#'@param Betriebsart operation type
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=1 is "timber forest, high forest"
##'@param Wuchsbezirk Growth district,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=13 is Central Alps
#''@param Bodentyp Soil type,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=9 is
#' "Heavy-textured Cambisols and Luvisols derived from moraine material,
#'  non-calcareous loess, or mudstone"
#'@param Schlagvegetation Presence(1) / absence(0) of clear cut vegetation
#'@param PL Period length in years, default=5 years, 5 years is maximum
#'recommended time step for the update of BAL and CCF
#'@param KGW Percentage attributed to forest in ANFI, default=10, "Plot entirely located in forest"
#'
#'@examples
#'JugendII.P(dg_cm=30,  Sum_G=40,  CCF=200)


JugendII.P<-function(dg_cm,  Sum_G,  CCF,  Seehoehe_m=600, Hangneigung_Proz=50,
                    J1=0,  J2=0,  Betriebsart=1, Wuchsbezirk=13,  Bodentyp=9,
                    Schlagvegetation=0,  PL=5,  KGW=10, Stoch=1){
a0 = 19.9104
a1 = -0.1689
a2 = -0.4188
a3 = 0.022
b1 = -35.8405
b2 = 0
b3 = -0.5538
b4 = 0.0037
b5 = 0.2178
b6 = 0
b7 = 0
b8 = 0
b9 = 1.0612
b10 = -0.5584
st1 = -0.1227
st2 = 0.00565
st3 = 0
g1 = -0.6219
g2 = 0.1607
g3 = -0.842
g4 = -0.246
g5 = -0.1534
g6 = 0.6158
s1 = -0.1569
s2 = 0
s3 = 0
s4 = 0
v1 = 0
v2 = 0
Schwelle = 0.34269
dWuchsbezirk1 = ifelse(Wuchsbezirk %in% c(8),1,0)
dWuchsbezirk2 = ifelse(Wuchsbezirk %in% c(9, 10),1,0)
dWuchsbezirk3 = ifelse(Wuchsbezirk %in% c(17),1,0)
dWuchsbezirk4 = ifelse(Wuchsbezirk %in% c(11, 12),1,0)
dWuchsbezirk5 = ifelse(Wuchsbezirk %in% c(13, 15),1,0)
dWuchsbezirk6 = ifelse(Wuchsbezirk %in% c(21),1,0)
dBodentyp1 = ifelse(Bodentyp %in% c(2, 3, 8, 9, 10),1,0)
dBodentyp2 = ifelse(Bodentyp %in% c(0), 1, 0)
dBodentyp3 = ifelse(Bodentyp %in% c(0), 1, 0)
dBodentyp4 = ifelse(Bodentyp %in% c(0), 1, 0)

Ausschlagwald<-ifelse(Betriebsart %in% c(6,7),1,0)

if(CCF==0) CCF<-1

JugIIt = 1 / (1 + exp(a0 + a1 * PL + a2 * KGW + a3 * KGW ^ 2 +b1 * (1 / log(dg_cm)) +
                b2 * (log(dg_cm)) + b3 * (log(dg_cm)) ^ 2 + b4 * (dg_cm * Sum_G) +
                b5 * log(CCF) + b6 * J1 + b7 * J2 + b8 * J2 * (log(CCF))+
                b9 * Ausschlagwald + b10 * Ausschlagwald * (log(CCF))+
                st1 * Seehoehe_m / 100 + st2 * (Seehoehe_m / 100) ^ 2 + st3 * Hangneigung_Proz+
                g1 * dWuchsbezirk1 +g2 * dWuchsbezirk1+g3 * dWuchsbezirk3 +
                g4 * dWuchsbezirk4 + g5 * dWuchsbezirk5 + g6 * dWuchsbezirk6+
                s1 * dBodentyp1 + s2 * dBodentyp2+s3 * dBodentyp3 + s4 * dBodentyp4+
                v1 * Schlagvegetation + v2 * Schlagvegetation * J1))


#Deterministisch
if(Stoch==0){
ifelse((JugIIt>=Schwelle & Sum_G>0),JugII<-1,JugII<-0)
  if(dg_cm>0){
    JugII<-0
  }
}

#Stochastisch
if(Stoch==1){
  ZZ = runif(1)
  ifelse((JugIIt>=ZZ & Sum_G>0),JugII<-1,JugII<-0)
  if(dg_cm>0){
    JugII<-0
  }
}
}

JugendII.P<-Vectorize(JugendII.P)

###########################################################################
#Einwuchspotential
###########################################################################
#'@title Potential for ingrowth for Prognaus
#'
#'@description \code{EWP.P} calculates the probability for ingrowth on a plot.
#'
#'@details
#'Calculates the probability for ingrowth on the plot. It can be applied stochastically or
#'deterministically. The model is part of the modelling system
#'for calculating ingrowth on plots developed by Ledermann (2002).
#'For full references see
#'\url{https://homepage.boku.ac.at/sonja/Lit_Prog.pdf}.
#'Coding of operation type, growth district and soil type uses the coding of the
#'Austrian National Forest Inventory
#'(\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}).
#'
#'
#'@param dg_cm Quadratic mean diameter in cm
#'@param sum_G Basal area m²/ha
#'@param CCF Crown competition factor (Krajicek, 1961)
#'@param Seehoehe_m Elevation in m (Range: 200-2200 m)
#'@param Hangneigung_Proz Slope in percent
#'@param J1  Presence(1) / absence(0) of regeneration I (height: 0m-1.3m)
#'@param J2 Presence(1) / absence(0) of regeneration II (height:1.3m-dbh=10.4cm)
#'@param Betriebsart operation type
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=1 is "timber forest, high forest"
#'@param Wuchsbezirk Growth district,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=13 is "Central Alps"
#''@param Bodentyp Soil type,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=9 is
#' "Heavy-textured Cambisols and Luvisols derived from moraine material,
#'  non-calcareous loess, or mudstone"
#'@param Schlagvegetation Presence(1) / absence(0) of clear-cut vegetation
#'@param PL Period length in years, default=5 years, 5 years is maximum
#'recommended time step for the update of BAL and CCF
#'@param KGW Percentage attributed to forest in ANFI, default=10, "Plot entirely located in forest"
#'@param Stoch Determines if the model is applied stochatically(1) or deterministically(0).
#'default=stochastic.
#'
#'@examples
#'EWP.P(dg_cm=30,  Sum_G=40,  CCF=200)


EWP.P<-function(dg_cm,  Sum_G,  CCF,  Seehoehe_m=600, Hangneigung_Proz=50,
              J1=0,  J2=0,  Betriebsart=1, Wuchsbezirk=13,  Bodentyp=9,
              Schlagvegetation=0,  PL=5,  KGW=10, Stoch=1){

if (Sum_G>0) {#Wenn Sum_G>0
  a0 = -0.7188
  a1 = -0.0509
  a2 = -0.883
  a3 = 0.0518
  b1 = 0
  b2 = 4.0471
  b3 = -0.5523
  b4 = 0.00122
  b5 = -0.061
  b6 = 0
  b7 = -1.9533
  b8 = 0.137
  b9 = 0.6121
  b10 = -0.3552
  st1 = 0
  st2 = 0
  st3 = 0
  g1 = 0.1808
  g2 = 0.3634
  g3 = -0.1632
  g4 = -0.2459
  g5 = -0.4227
  g6 = 0
  s1 = 0.9728
  s2 = -0.2876
  s3 = 0.232
  s4 = 0.163
  v1 = 0
  v2 = 0
  Schwelle = 0.31832
  dWuchsbezirk1 = ifelse(Wuchsbezirk %in% c(6, 9),1,0)
  dWuchsbezirk2 = ifelse(Wuchsbezirk %in% c(10),1,0)
  dWuchsbezirk3 = ifelse(Wuchsbezirk %in% c(13, 18),1,0)
  dWuchsbezirk4 = ifelse(Wuchsbezirk %in% c(15),1,0)
  dWuchsbezirk5 = ifelse(Wuchsbezirk %in% c(19),1,0)
  dWuchsbezirk6 = ifelse(Wuchsbezirk %in% c(0),1,0)
  dBodentyp1 = ifelse(Bodentyp %in% c(7),1,0)
  dBodentyp2 = ifelse(Bodentyp %in% c(10),1,0)
  dBodentyp3 = ifelse(Bodentyp %in% c(18),1,0)
  dBodentyp4 = ifelse(Bodentyp %in% c(21, 22),1,0)
}

if (Sum_G==0) {#Wenn Sum_G==0
  a0 = 11.7296
  a1 = -0.4102
  a2 = -1.4348
  a3 = 0.0738
  b1 = 0
  b2 = 0
  b3 = 0
  b4 = 0
  b5 = 0
  b6 = -1.4117
  b7 = -3.0197
  b8 = 0
  b9 = 1.0515
  b10 = 0
  st1 = 0
  st2 = 0.00496
  st3 = 0.00874
  g1 = 0.194
  g2 = -0.8389
  g3 = 0
  g4 = 0
  g5 = 0
  g6 = 0
  s1 = -0.3211
  s2 = 0.5052
  s3 = 0
  s4 = 0
  v1 = -0.6854
  v2 = 0.5024
  Schwelle = 0.4083
  dWuchsbezirk1 = ifelse(Wuchsbezirk %in% c(6, 9, 10),1,0)
  dWuchsbezirk2 = ifelse(Wuchsbezirk %in% c(17),1,0)
  dWuchsbezirk3 = ifelse(Wuchsbezirk %in% c(0), 1, 0)
  dWuchsbezirk4 = ifelse(Wuchsbezirk %in% c(0), 1, 0)
  dWuchsbezirk5 = ifelse(Wuchsbezirk %in% c(0), 1, 0)
  dWuchsbezirk6 = ifelse(Wuchsbezirk %in% c(0), 1, 0)
  dBodentyp1 = ifelse(Bodentyp %in% c(2, 3, 4, 5, 6, 7, 8, 9, 10),1,0)
  dBodentyp2 = ifelse(Bodentyp %in% c(21, 22),1,0)
  dBodentyp3 = ifelse(Bodentyp %in% c(0), 1, 0)
  dBodentyp4 = ifelse(Bodentyp %in% c(0), 1, 0)
}


Ausschlagwald<-ifelse(Betriebsart %in% c(6,7),1,0)

if(CCF==0) CCF<-1
if(dg_cm==0) dg_cm<-2

Seehoehe = Seehoehe_m / 100

Logit_Einwuchs = a0 + a1 * PL + a2 * KGW + a3 * KGW ^ 2 + b1 * (1 / log(dg_cm))+
b2 * (log(dg_cm)) + b3 * (log(dg_cm)) ^ 2 + b4 * (dg_cm * Sum_G)+
b5 * log(CCF) + b6 * J1 + b7 * J2 + b8 * J2 * (log(CCF))+
b9 * Ausschlagwald + b10 * Ausschlagwald * (log(CCF)) +
st1 * Seehoehe + st2 * Seehoehe ^ 2 + st3 * Hangneigung_Proz+
g1 * dWuchsbezirk1 + g2 * dWuchsbezirk2 + g3 * dWuchsbezirk3 +
g4 * dWuchsbezirk4 + g5 * dWuchsbezirk5 + g6 * dWuchsbezirk6 +
s1 * dBodentyp1 + s2 * dBodentyp2+ s3 * dBodentyp3 + s4 * dBodentyp4+
v1 * Schlagvegetation + v2 * Schlagvegetation * J1

WS_Einwuchs <- 1 / (1 + exp(Logit_Einwuchs))

#Anheben der Einwuchswahrscheinlichkeit, da in Simulation G nicht null
#if (Sum_G<4){
  #WS_Einwuchs = WS_Einwuchs * 4
#}

ZZ = runif(1)
EWPot<-0
if((Stoch==0 & WS_Einwuchs>Schwelle) | (Stoch==1 & WS_Einwuchs>ZZ)){
  EWPot<-1
}
EWPot
}

EWP.P<-Vectorize(EWP.P)

#####################################################################################
#Einwuchsanzahl
#####################################################################################
#'@title Number of ingrowing trees on a plot
#'
#'@description \code{EW.Anzahl.P} calculates the number of ingrowing trees on a plot
#'in a 5-year period
#'
#'@details
#'Calculates the number of ingrowing trees on a plot, if there is presence of ingrowth predicted by
#'\code{\link{EWP.P}}. The model is part ot the modelling system
#'for calculating ingrowth on plots developed by Ledermann (2002).
#'For full references see
#'\url{https://homepage.boku.ac.at/sonja/Lit_Prog.pdf}.
#'Coding of operation type and growth district uses the coding of the
#'Austrian National Forest Inventory
#'(\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}).
#'
#'
#'@param dg_cm Quadratic mean diameter in cm
#'@param sum_G Basal area m²/ha
#'@param CCF Crown competition factor (Krajicek, 1961)
#'@param Seehoehe_m Elevation in m (Range: 200-2200 m)
#'@param Hangneigung_Proz Slope in percent
#'@param Exposition_Grad Aspect in degree
#'@param J1 Presence(1) / absence(0) of regeneration I (height: 0m-1.3m)
#'@param J2 Presence(1) / absence(0) of regeneration II (height:1.3m-dbh=10.4cm)
#'@param Betriebsart operation type
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=1 is "timber forest, high forest"
#'@param Wuchsbezirk Growth district,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=13 is "Central Alps"
#'@param PL Period length in years, default=5 years, 5 years is maximum
#'recommended time step for the update of BAL and CCF
#'@param KGW Percentage attributed to forest in ANFI, default=10, "Plot entirely located in forest"
#'
#'@examples
#'EW.Anzahl.P(dg_cm=30,  Sum_G=40,  CCF=200)


EW.Anzahl.P<-function(dg_cm,  Sum_G,  CCF,  Seehoehe_m=600, Hangneigung_Proz=50,
                    Exposition_Grad=180,  J1=0,  J2=0, Betriebsart=1,
                    Wuchsbezirk=13,  PL=5,  KGW=10){
a0 = -0.71716
a1 = 0.066166
a2 = 0.054476
b1 = 0.790126
b2 = 3.435201
b3 = -0.00486
b4 = 0.388759
b5 = -0.02565
b6 = 0.084276
st1 = -0.03265
st2 = 0.001335
st3 = -0.00054
st4 = -0.00254
st5 = 0.001574
g1 = -0.19172
dWuchsbezirk1 = ifelse(Wuchsbezirk %in% c(9),1,0)
Ausschlagwald<-ifelse(Betriebsart %in% c(6,7),1,0)
BEST<-ifelse(Sum_G>0,1,0)

if(CCF==0) CCF<-1
if(dg_cm==0) dg_cm<-2

Seehoehe <- Seehoehe_m / 100
Exposition_rad <- (Exposition_Grad / 180) * pi

n <- exp(a0 + a1 * PL + a2 * KGW + b1 * (1 - BEST) +
        b2 * (BEST / (log(dg_cm)) ^ 3) + b3 * BEST * Sum_G + b4 * J2 +
        b5 * BEST * J2 * log(CCF) + b6 * BEST * Ausschlagwald * log(CCF)+
        st1 * (1 - BEST) * Seehoehe + st2 * Hangneigung_Proz * cos(Exposition_rad)+
        st3 * Hangneigung_Proz * sin(Exposition_rad) + st4 * (1 - BEST) * Hangneigung_Proz * cos(Exposition_rad)+
        st5 * (1 - BEST) * Hangneigung_Proz * sin(Exposition_rad) + g1 * dWuchsbezirk1)

n<-round(n,0) #Ganze Anzahl an Bäumen

}

EW.Anzahl.P<-Vectorize(EW.Anzahl.P)


######################################################################################
#Baumart des Einwuchses
######################################################################################
#'@title Probability of tree species
#'
#'@description \code{EinwuchsBA} calculates the probability of presence/absence for a certain tree species
#'
#'@details
#'Calculates the probability of presence/absence for a certain tree species.
#'During the simulation the caluculation needs to be done for each of the 13 tree species or tree species groups
#'and for the number of ingrowing trees predicted by \code{\link{EW.Anzahl.P}}. The choice of tree species is
#'determined by the call in the ingrowth routine.
#'The model is part ot the modelling system
#'for calculating ingrowth on plots developed by Ledermann (2002).
#'For full references see
#'\url{https://homepage.boku.ac.at/sonja/Lit_Prog.pdf}.
#'Coding of tree species, soil moisture, growth district, soil type and operation type uses the coding of the
#'Austrian National Forest Inventory
#'(\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}).
#'
#'
#'@param BART Tree species for which the probability of ingrowth is predicted
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI},
#'default=1 Norway spruce#'
#'@param sum_G Basal area m²/ha
#'@param CCF Crown competition factor (Krajicek, 1961)
#'@param Seehoehe_m Elevation in m (Range: 200-2200 m)
#'@param Hangneigung_Proz Slope in percent
#'@param Exposition_Grad Aspect in degree
#'@param Bodenfeuchte Soil moisture,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=3 is "moist"
#'@param Wuchsbezirk Growth district,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=13 is "Central Alps"
#'@param Bodentyp Soil type,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=9 is
#'"Heavy-textured Cambisols and Luvisols derived from moraine material,
#'non-calcareous loess, or mudstone"
#'@param Betriebsart operation type
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=1 is "timber forest, high forest"
#'@param species.vec Vector with species present on plot
#'
#'
#'@examples
#'EinwuchsBA.P(Sum_G=40,  CCF=200)

EinwuchsBA.P<-function(BART=1, Sum_G, CCF, Seehoehe_m=600, Hangneigung_Proz=50, Exposition_Grad=180,
                       Bodenfeuchte=3, Wuchsbezirk=13, Bodentyp=9, Betriebsart=1,
                       spec.vec=c(1,10)){

  if (BART==1) { #Fichte
    a0 = 1.4957
    b1 = -1.5755
    b2 = 0.9461
    b3 = 0
    b4 = 0
    b5 = -0.552
    b6 = 0
    b7 = 0
    b8 = 0
    b9 = 0.9019
    b10 = -0.2515
    b11 = 0.0745
    b12 = 0
    st1 = -0.553
    st2 = 0.022
    st3 = 0.0146
    st4 = 0
    st5 = 0
    st6 = -0.5281
    st7 = 0
    st8 = 0
    st9 = 0
    st10 = 0.00629
    st11 = -0.00266
    g1 = 0
    g2 = 0
    g3 = 0
    g4 = 0
    s1 = -0.5998
    s2 = -0.5268
    s3 = 2.3416
    gs1 = 1.5647
    gs2 = -0.2308
    gs3 = -0.1818
    gs4 = 0.0128
    gs5 = 0
    dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(2,21), 1, 0)
    dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(11, 12, 13, 14, 15), 1, 0)
    dWuchsbezirk3<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk4<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dBodentyp1<-ifelse(Bodentyp %in% c(4, 5, 6, 7), 1, 0)
    dBodentyp2<-ifelse(Bodentyp %in% c(11, 12, 13, 14), 1, 0)
    dBodentyp3<-ifelse(Bodentyp %in% c(21, 22), 1, 0)
  }

  if (BART==2){#Tanne
    a0 = 7.8984
    b1 = -5.2211
    b2 = 0
    b3 = 0
    b4 = 2.0273
    b5 = 0
    b6 = 0
    b7 = 0
    b8 = 0
    b9 = 0
    b10 = 1.6085
    b11 = -0.3528
    b12 = 0
    st1 = 0
    st2 = 0
    st3 = 0
    st4 = 0
    st5 = 2.419
    st6 = 0
    st7 = 0
    st8 = 0.00653
    st9 = -0.0399
    st10 = 0
    st11 = 0
    g1 = 19.1533
    g2 = 3.9503
    g3 = 0
    g4 = 0
    s1 = -3.0308
    s2 = 0
    s3 = 0
    gs1 = -8.0549
    gs2 = 0.6894
    gs3 = -0.4402
    gs4 = 0
    gs5 = 0.0373
    dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(18, 19), 1, 0)
    dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(1), 1, 0)
    dWuchsbezirk3<-ifelse(Wuchsbezirk %in% c(11, 12, 13, 14, 15), 1, 0)
    dWuchsbezirk4<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dBodentyp1<-ifelse(Bodentyp %in% c(2, 4, 5, 6, 7), 1, 0)
    dBodentyp2<-ifelse(Bodentyp %in% c(0), 1, 0)
    dBodentyp3<-ifelse(Bodentyp %in% c(0), 1, 0)
  }

  if (BART==3){#Laerche
    a0 = 5.2292
    b1 = -1.2775
    b2 = 0
    b3 = 0
    b4 = 0
    b5 = 0
    b6 = 0
    b7 = 0
    b8 = 0
    b9 = 0
    b10 = -0.8254
    b11 = 0.2178
    b12 = 0
    st1 = 0
    st2 = -0.0121
    st3 = 0
    st4 = 0
    st5 = 0.1966
    st6 = -0.9353
    st7 = 0
    st8 = 0.0109
    st9 = -0.0214
    st10 = -0.0198
    st11 = 0.0133
    g1 = -0.8498
    g2 = 0
    g3 = 0
    g4 = 0
    s1 = -0.384
    s2 = 0
    s3 = 0
    gs1 = 0
    gs2 = 0
    gs3 = 0
    gs4 = 0
    gs5 = 0
    dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(6, 9, 10, 17), 1, 0)
    dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk3<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk4<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dBodentyp1<-ifelse(Bodentyp %in% c(1, 17), 1, 0)
    dBodentyp2<-ifelse(Bodentyp %in% c(0), 1, 0)
    dBodentyp3<-ifelse(Bodentyp %in% c(0), 1, 0)
  }

  if (BART==4){#Kiefer
    a0 = 2.7983
    b1 = -2.1753
    b2 = 0
    b3 = 0
    b4 = 0
    b5 = 0
    b6 = 1.0578
    b7 = 0
    b8 = 0
    b9 = 0.3548
    b10 = -0.7317
    b11 = 0.2331
    b12 = 0
    st1 = 0
    st2 = 0.0161
    st3 = 0
    st4 = 0.2347
    st5 = -0.3266
    st6 = -0.272
    st7 = 0
    st8 = 0
    st9 = 0
    st10 = 0
    st11 = 0
    g1 = 0
    g2 = 0
    g3 = 0
    g4 = 0
    s1 = -1.7676
    s2 = -0.5574
    s3 = 0
    gs1 = -0.1997
    gs2 = 0
    gs3 = 0
    gs4 = 0
    gs5 = 0
    dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(2, 19, 20, 21), 1, 0)
    dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk3<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk4<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dBodentyp1<-ifelse(Bodentyp %in% c(23), 1, 0)
    dBodentyp2<-ifelse(Bodentyp %in% c(4, 5, 6, 7), 1, 0)
    dBodentyp3<-ifelse(Bodentyp %in% c(0), 1, 0)
  }

  if (BART==6){#Zirbe
    a0 = 18.2535
    b1 = -3.7033
    b2 = 0
    b3 = 0
    b4 = 0
    b5 = 0
    b6 = 0
    b7 = 0
    b8 = 0
    b9 = 0
    b10 = 0.518
    b11 = 0
    b12 = 0
    st1 = -0.9344
    st2 = 0
    st3 = 0
    st4 = 0
    st5 = -0.3345
    st6 = 0
    st7 = 0
    st8 = 0
    st9 = 0
    st10 = 0
    st11 = 0
    g1 = 0
    g2 = 0
    g3 = 0
    g4 = 0
    s1 = 0
    s2 = 0
    s3 = 0
    gs1 = 0
    gs2 = 0
    gs3 = 0
    gs4 = 0
    gs5 = 0
    dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk3<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk4<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dBodentyp1<-ifelse(Bodentyp %in% c(0), 1, 0)
    dBodentyp2<-ifelse(Bodentyp %in% c(0), 1, 0)
    dBodentyp3<-ifelse(Bodentyp %in% c(0), 1, 0)
  }

  if (BART==10) {#Buche
    a0 = 3.0713
    b1 = -2.3222
    b2 = 0
    b3 = -0.5162
    b4 = 0
    b5 = 0
    b6 = 0.3037
    b7 = 0
    b8 = 0
    b9 = 0
    b10 = 0
    b11 = 0
    b12 = 1.1974
    st1 = -0.3232
    st2 = 0.0271
    st3 = 0
    st4 = 0.7313
    st5 = 0.9379
    st6 = 0.9573
    st7 = 0
    st8 = 0
    st9 = 0
    st10 = 0
    st11 = 0
    g1 = 42.1947
    g2 = 0.3899
    g3 = -1.7217
    g4 = -0.4746
    s1 = 1.1992
    s2 = -1.4914
    s3 = -0.9918
    gs1 = -9.4536
    gs2 = 0.4971
    gs3 = 0
    gs4 = 0.9379
    gs5 = 0
    dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(17), 1, 0)
    dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(1), 1, 0)
    dWuchsbezirk3<-ifelse(Wuchsbezirk %in% c(6), 1, 0)
    dWuchsbezirk4<-ifelse(Wuchsbezirk %in% c(9,10), 1, 0)
    dBodentyp1<-ifelse(Bodentyp %in% c(4), 1, 0)
    dBodentyp2<-ifelse(Bodentyp %in% c(11), 1, 0)
    dBodentyp3<-ifelse(Bodentyp %in% c(18), 1, 0)
  }

  if (BART==11){#Eiche
    a0 = 1.7228
    b1 = -1.9924
    b2 = 0
    b3 = 0
    b4 = 0
    b5 = 0
    b6 = 0
    b7 = 1.8058
    b8 = 0
    b9 = 0
    b10 = 0.159
    b11 = 0
    b12 = 0
    st1 = 0.5
    st2 = 0
    st3 = 0.0653
    st4 = 0.5913
    st5 = 0.2758
    st6 = 0
    st7 = 0
    st8 = 0
    st9 = 0
    st10 = 0.0314
    st11 = 0.0632
    g1 = -1.6264
    g2 = 0
    g3 = 0
    g4 = 0
    s1 = -0.9511
    s2 = 0
    s3 = 0
    gs1 = 0
    gs2 = 0
    gs3 = -0.3989
    gs4 = 0.2758
    gs5 = 0
    dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(7, 8), 1, 0)
    dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(2, 21), 1, 0)
    dWuchsbezirk3<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk4<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dBodentyp1<-ifelse(Bodentyp %in% c(2, 3, 8, 9, 10, 11, 12, 13, 14), 1, 0)
    dBodentyp2<-ifelse(Bodentyp %in% c(0), 1, 0)
    dBodentyp3<-ifelse(Bodentyp %in% c(0), 1, 0)
  }

  if (BART==13) {#Esche
    a0 = 5.2609
    b1 = -3.2784
    b2 = 0
    b3 = 0
    b4 = 0
    b5 = 0
    b6 = 0
    b7 = 0
    b8 = 0
    b9 = 0.6211
    b10 = -0.374
    b11 = 0.0467
    b12 = 0.5244
    st1 = -0.4186
    st2 = 0.0453
    st3 = 0
    st4 = -0.4929
    st5 = 0
    st6 = 0
    st7 = -1.0744
    st8 = 0
    st9 = 0
    st10 = 0
    st11 = 0
    g1 = 0
    g2 = 0
    g3 = 0
    g4 = 0
    s1 = -1.9575
    s2 = 0
    s3 = 0
    gs1 = 0
    gs2 = 0
    gs3 = 0
    gs4 = 0
    gs5 = 0
    dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk3<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk4<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dBodentyp1<-ifelse(Bodentyp %in% c(21, 22), 1, 0)
    dBodentyp2<-ifelse(Bodentyp %in% c(0), 1, 0)
    dBodentyp3<-ifelse(Bodentyp %in% c(0), 1, 0)
  }

  if (BART==14){#Ahorn (Ulme)
    a0 = 4.1801
    b1 = -1.8122
    b2 = 0
    b3 = 0
    b4 = 0
    b5 = 0
    b6 = 0
    b7 = 0
    b8 = 0
    b9 = 0
    b10 = -0.962
    b11 = 0.1617
    b12 = 0
    st1 = 0.9154
    st2 = -0.0352
    st3 = -0.028
    st4 = 0
    st5 = 0
    st6 = 0
    st7 = -2.3576
    st8 = 0
    st9 = 0
    st10 = 0
    st11 = 0
    g1 = -1.0844
    g2 = 0
    g3 = 0
    g4 = 0
    s1 = -1.5114
    s2 = -2.2904
    s3 = 0
    gs1 = 0
    gs2 = 0
    gs3 = 0
    gs4 = 0
    gs5 = 0
    dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(9), 1, 0)
    dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk3<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk4<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dBodentyp1<-ifelse(Bodentyp %in% c(1, 2), 1, 0)
    dBodentyp2<-ifelse(Bodentyp %in% c(18, 19), 1, 0)
    dBodentyp3<-ifelse(Bodentyp %in% c(0), 1, 0)
  }

  if (BART==12) {#Hainbuche, Linde, (Kastanie, Robinie)
    a0 = 6.6962
    b1 = -4.3798
    b2 = 0
    b3 = 0
    b4 = 0
    b5 = 0
    b6 = 0
    b7 = 0
    b8 = -0.5916
    b9 = 0.5654
    b10 = -0.1039
    b11 = 0
    b12 = 0
    st1 = -2.1691
    st2 = 0.3877
    st3 = 0
    st4 = 0.8212
    st5 = 0.3242
    st6 = 0
    st7 = 0
    st8 = 0.0263
    st9 = -0.0003
    st10 = 0
    st11 = 0
    g1 = -1.4995
    g2 = -2.4052
    g3 = 0
    g4 = 0
    s1 = 0
    s2 = 0
    s3 = 0
    gs1 = 0
    gs2 = 0
    gs3 = 0
    gs4 = 0
    gs5 = 0
    dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(2, 19, 20, 21), 1, 0)
    dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(8, 9), 1, 0)
    dWuchsbezirk3<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk4<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dBodentyp1<-ifelse(Bodentyp %in% c(0), 1, 0)
    dBodentyp2<-ifelse(Bodentyp %in% c(0), 1, 0)
    dBodentyp3<-ifelse(Bodentyp %in% c(0), 1, 0)
  }

  if (BART==21){#Erle
    a0 = 2.8537
    b1 = -3.357
    b2 = 0
    b3 = 0
    b4 = 0
    b5 = 0
    b6 = 0
    b7 = 0
    b8 = 0
    b9 = 0
    b10 = 0.2942
    b11 = 0
    b12 = 0
    st1 = 0.2017
    st2 = -0.0104
    st3 = 0
    st4 = -1.1329
    st5 = 0.6877
    st6 = 0
    st7 = -0.9015
    st8 = 0
    st9 = 0
    st10 = 0
    st11 = 0
    g1 = 0
    g2 = 0
    g3 = 0
    g4 = 0
    s1 = -1.7732
    s2 = -1.4835
    s3 = 0
    gs1 = 0
    gs2 = 0
    gs3 = 0
    gs4 = 0
    gs5 = 0
    dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk3<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk4<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dBodentyp1<-ifelse(Bodentyp %in% c(21, 22), 1, 0)
    dBodentyp2<-ifelse(Bodentyp %in% c(23), 1, 0)
    dBodentyp3<-ifelse(Bodentyp %in% c(0), 1, 0)
  }

  if (BART==20){#Birke Sorbus
    a0 = 3.3401
    b1 = -2.0522
    b2 = 0
    b3 = 0
    b4 = 0
    b5 = 0
    b6 = 0
    b7 = 0
    b8 = 0
    b9 = -0.3914
    b10 = 0
    b11 = 0.014
    b12 = -0.3572
    st1 = 0
    st2 = 0.0149
    st3 = 0
    st4 = 0.1895
    st5 = -0.2083
    st6 = -0.6394
    st7 = -1.4328
    st8 = 0.00306
    st9 = -0.013
    st10 = 0
    st11 = 0
    g1 = 0
    g2 = 0
    g3 = 0
    g4 = 0
    s1 = -0.3696
    s2 = 0
    s3 = 0
    gs1 = 0
    gs2 = 0
    gs3 = 0
    gs4 = 0
    gs5 = 0
    dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk3<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk4<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dBodentyp1<-ifelse(Bodentyp %in% c(1, 17, 21, 22), 1, 0)
    dBodentyp2<-ifelse(Bodentyp %in% c(0), 1, 0)
    dBodentyp3<-ifelse(Bodentyp %in% c(0), 1, 0)
  }

  if (BART==24){#Pappel, Weide
    a0 = 3.0537
    b1 = -3.4703
    b2 = 0
    b3 = 0
    b4 = 0
    b5 = 0
    b6 = 0
    b7 = 0
    b8 = 0
    b9 = 0.4416
    b10 = -0.7158
    b11 = 0.2049
    b12 = 1.5484
    st1 = 0.1776
    st2 = 0
    st3 = 0
    st4 = 0
    st5 = 0
    st6 = 0
    st7 = -0.8514
    st8 = 0
    st9 = 0
    st10 = 0
    st11 = 0
    g1 = -0.5625
    g2 = 0
    g3 = 0
    g4 = 0
    s1 = -1.1917
    s2 = 0
    s3 = 0
    gs1 = 0
    gs2 = 0
    gs3 = 0
    gs4 = 0
    gs5 = 0
    dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(2, 19, 20, 21), 1, 0)
    dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk3<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dWuchsbezirk4<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
    dBodentyp1<-ifelse(Bodentyp %in% c(1, 17, 21, 22), 1, 0)
    dBodentyp2<-ifelse(Bodentyp %in% c(0), 1, 0)
    dBodentyp3<-ifelse(Bodentyp %in% c(0), 1, 0)
  }

  Seehoehe = Seehoehe_m / 100
  Hangneigung = Hangneigung_Proz
  Exposition_rad = (Exposition_Grad / 180) * pi
  BEST<-ifelse(Sum_G>0, 1, 0)
  if(CCF==0) CCF<-1

  KahlTro<-ifelse(Sum_G==0 & Bodenfeuchte %in% c(1,2), 1, 0)
  KahlFeu<-ifelse(Sum_G==0 & Bodenfeuchte %in% c(4,5), 1, 0)
  WR = Bodenfeuchte - 3
  Ausschlagwald<-ifelse(Betriebsart %in% c(6,7),1,0)

  specPres<-ifelse(BART %in% spec.vec,1,0)
  FiPres<-ifelse(1 %in% spec.vec,1,0)
  TaPres<-ifelse(2 %in% spec.vec,1,0)
  KiPres<-ifelse(4 %in% spec.vec,1,0)
  BuPres<-ifelse(10 %in% spec.vec,1,0)
  EiPres<-ifelse(11 %in% spec.vec,1,0)
  HBPres<-ifelse(12 %in% spec.vec,1,0)
  TaBuPres<-ifelse(2 %in% spec.vec,1,0)
  TaBuPres<-ifelse(10 %in% spec.vec,1,0)

  Logit_Baumart = a0 + b1 * specPres + b2 * TaBuPres + b3 * TaPres + b4 * BuPres + b5 * KiPres+
    b6 * FiPres + b7 * HBPres + b8 * EiPres + b9 * BEST + b10 * BEST * log(CCF)+
    b11 * BEST * (log(CCF)) ^ 2 + b12 * Ausschlagwald+
    st1 * Seehoehe + st2 * Seehoehe ^ 2 + st3 * Hangneigung + st4 * WR + st5 * WR ^ 2+
    st6 * KahlTro + st7 * KahlFeu + st8 * Hangneigung * sin(Exposition_rad) +
    st9 * Hangneigung * cos(Exposition_rad) + st10 * BEST * Hangneigung * sin(Exposition_rad)+
    st11 * BEST * Hangneigung * cos(Exposition_rad)+
    g1 * dWuchsbezirk1 + g2 * dWuchsbezirk2 +
    g3 * dWuchsbezirk3 + g4 * dWuchsbezirk4 +
    s1 * dBodentyp1 + s2 * dBodentyp2 + s3 * dBodentyp3+
    gs1 * dWuchsbezirk1 * Seehoehe + gs2 * dWuchsbezirk1 * Seehoehe ^ 2+
    gs3 * dWuchsbezirk2 * Seehoehe + gs4 * dWuchsbezirk2 * Seehoehe ^ 2+
    gs5 * dWuchsbezirk3 * Seehoehe ^ 2

  Einwuchs_Baumart = 1 / (1 + exp(Logit_Baumart))
}

#Einwuchs_Baumart Modell nicht vektorisieren!!!

######################################################################################
#BHD des Einwuchses
######################################################################################
#'@title Diameter of ingrowing trees Prognaus
#'
#'@description \code{EW.BHD.P} calculates the diameter of ingrowing trees
#'
#'@details
#'Calculates the diameter for ingrowing trees
#'The model is part ot the modelling system
#'for calculating ingrowth on plots developed by Ledermann (2002).
#'For full references see
#'\url{https://homepage.boku.ac.at/sonja/Lit_Prog.pdf}.
#'
#'
#'@param Anzahl_Einwuchs Number of ingrowing trees calculated by \code{\link{EW.Anzahl.P}}.
#'@param sum_G Basal area m²/ha
#'
#'@examples EW.BHD.P(Anzahl_Einwuchs=5,  Sum_G40)

EW.BHD.P<-function(Anzahl_Einwuchs,  Sum_G){

a0 = 5.68579
b0 = -0.57292
b1 = 0.149665
c0 = 0.559611
c1 = 0.00222

FBHD = runif(1)

Einwuchs_BHD = a0 + (exp(b0 + b1 * log(Anzahl_Einwuchs))) * exp((log(-log(1 - FBHD)))/
               (exp(c0 + c1 * Sum_G)))
}

EW.BHD.P<-Vectorize(EW.BHD.P)
######################################################################################
#Höhe des Einwuchses
######################################################################################
#'@title Height of ingrowing trees for Prognaus
#'
#'@description \code{EWH.P} calculates the height for ingrowing trees
#'
#'@details
#'Calculates the height of ingrowing trees in m. The model is part ot the modelling system
#'for calculating ingrowth on plots developed by Ledermann (2002).
#'For full references see
#'\url{https://homepage.boku.ac.at/sonja/Lit_Prog.pdf}.
#'Coding of tree species of ingrowing trees and relief uses the coding of the
#'Austrian National Forest Inventory
#'(\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}).
#'
#'
#'@param BART_EW Tree species of ingrowing tree,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI},
#'default=1 Norway spruce
#'@param BHD_EW_cm Diameter of ingrowing tree in cm
#'@param Seehoehe_m Elevation in m (Range: 200-2200 m)
#'@param dg_cm Quadratic mean diameter in cm
#'@param Sum_G Basal area m²/ha
#'@param Sum_G10 Basal area od trees with a diamter > 10 cm, in m²/ha
#'@param CCF Crown competition factor (Krajicek, 1961)
#'@param Relief Slope position,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=2 is "middle slope"
#'
#'
#'@examples
#'EWH.P(BHD_EW_cm=6,  dg_cm=10,
#'Sum_G=40,  sum_G10=38,  CCF=200,  Relief=2)


EWH.P<-function(BART_EW=1,  BHD_EW_cm,  Seehoehe_m=600,  dg_cm,
              Sum_G,  sum_G10,  CCF,  Relief=2){

if (BART_EW==1 | BART_EW==2) { #Fichte Tanne, sonst_Nadelholz
  a0 = 0.298884
  a1 = -0.0123
  a2 = -0.03235
  a3 = 0
  a4 = -0.00897
  a5 = -0.07624
  a6 = 0.343915
  a7 = 0.013896
  a8 = -0.04392
  a9 = 0
  b0 = 0.782562
  b1 = 0.127848
  b2 = 0
  b3 = 0
  b4 = 0
  b5 = 0
  b6 = -0.10051
  b7 = -0.00151
  b8 = 0
  b9 = 0.015379
  b10 = 0.004878
}

if (BART_EW %in% c(3:6)) {#Lärche, Kiefer, Schwarzkiefer,Zirbe
  a0 = 0.789479
  a1 = -0.02261
  a2 = 0
  a3 = 0
  a4 = 0
  a5 = 0
  a6 = 0
  a7 = 0.024428
  a8 = -0.01486
  a9 = 0
  b0 = 0.607519
  b1 = 0
  b2 = -0.04675
  b3 = 0
  b4 = 0
  b5 = 0
  b6 = 0
  b7 = 0
  b8 = 0
  b9 = 0
  b10 = 0
}

if (BART_EW %in% c(10,12,16,23)){#Buche, Hainbuche, (Kastanie), Linde
  a0 = 0.474286
  a1 = -0.01924
  a2 = 0
  a3 = 0
  a4 = 0
  a5 = -0.15275
  a6 = 0.189286
  a7 = 0
  a8 = 0
  a9 = 0
  b0 = 0.802
  b1 = 0
  b2 = 0
  b3 = 0
  b4 = 0
  b5 = 0
  b6 = 0
  b7 = -0.00362
  b8 = 0
  b9 = 0.00203
  b10 = 0
}

if (BART_EW %in% c(11,13,14,15,17)) {#11, 13, 14, 15, 17 Eiche, Esche, Ahorn, Ulme, (Robinie)
  a0 = 1.274788
  a1 = 0
  a2 = 0
  a3 = 0.146028
  a4 = 0
  a5 = 0
  a6 = -0.91601
  a7 = -0.0856
  a8 = 0
  a9 = 0.392387
  b0 = 0.425585
  b1 = 0
  b2 = 0
  b3 = -0.16717
  b4 = -0.09304
  b5 = 0
  b6 = 0.49957
  b7 = 0
  b8 = 0.055938
  b9 = -0.00883
  b10 = -0.21516
}

if (BART_EW %in% c(18, 20, 21, 22, 24, 25, 26, 27, 28)) {#Sorbus,Birke, Erle, Pappel, Weide
  a0 = 1.142313
  a1 = -0.02645
  a2 = 0
  a3 = 0
  a4 = 0
  a5 = -0.16795
  a6 = 0
  a7 = 0
  a8 = 0
  a9 = 0
  b0 = 0.548174
  b1 = 0
  b2 = 0
  b3 = 0
  b4 = 0
  b5 = -0.02766
  b6 = 0
  b7 = 0
  b8 = 0.001453
  b9 = 0
  b10 = 0
}

if(CCF==0) CCF<-1
BEST<-ifelse(Sum_G>0,1,0)
OH<-ifelse(Relief==1,1,0)
Seehoehe = Seehoehe_m / 100

Ta<-ifelse(BART_EW==2,1,0)
Ki<-ifelse(BART_EW==4,1,0)
Ei<-ifelse(BART_EW==11,1,0)
Ah<-ifelse(BART_EW==14,1,0)
Erle<-ifelse(BART_EW==21,1,0)

Oberhang<-ifelse(Relief==1,1,0)

Einwuchs_Hoehe <- exp(a0 + a1 * Seehoehe + a2 * Ta * Seehoehe + a3 * BEST * Ei +
                  a4 * BEST * Seehoehe+a5 * Oberhang + a6 * BEST + a7 * Sum_G +
                  a8 * sum_G10 +a9 * log(CCF)+b0 * log(BHD_EW_cm) +
                  b1 * Ta * log(BHD_EW_cm) + b2 * Ki * log(BHD_EW_cm) +
                  b3 * Ei * log(BHD_EW_cm) + b4 * Ah * log(BHD_EW_cm)+
                  b5 * Erle * log(BHD_EW_cm) + b6 * BEST * log(BHD_EW_cm) +
                  b7 * dg_cm * log(BHD_EW_cm)+b8 * Sum_G * log(BHD_EW_cm)+
                  b9 * sum_G10 * log(BHD_EW_cm) + b10 * log(CCF) * log(BHD_EW_cm))

}

EWH.P<-Vectorize(EWH.P)

#########################################################################################
#Einwuchsmodell
########################################################################################
#'@title Ingrowth model of Prognaus
#'
#'@description \code{Einwuchs.P} calculates the ingrowing trees for a plot; If no ingrowth
#'is predicted the function returns "0", otherwise a matrix of ingrowing trees.
#'Needs to be applied for each plot
#'
#'@details
#'Calculates the ingrowing trees of a plot. The routine calls the following sub-models:
#'\code{\link{JugendII.P}}: a model to calculate the presence/absence of regeneration II
#'\code{\link{EWP.P}}: a model to calculate the presence/absence of ingrowth
#'\code{\link{EW.Anzahl.P}}: a model to calculate the number of ingrowing trees, if ingrowth is present
#'\code{\link{EinwuchsBA.P}}: a model to determine the tree species of ingrowing trees
#'\code{\link{EW.BHD.P}}: a model to determine the diameter of ingrowing trees
#'\code{\link{EWH.P}}: a model to determine the height of ingrowing trees
#'All models were developed by Ledermann (2002).
#'For full references see
#'\url{https://homepage.boku.ac.at/sonja/Lit_Prog.pdf}.
#'Coding of operation type, growth district and soil type uses the coding of the
#'Austrian National Forest Inventory
#'(\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}).
#'
#'
#'@param dg_cm Quadratic mean diameter in cm
#'@param sum_G Basal area m²/ha
#'@param sum_G10 Basal area m²/ha for trees with a dbh>10cm
#'@param CCF Crown competition factor (Krajicek, 1961)
#'@param Seehoehe_m Elevation in m (Range: 200-2200 m)
#'@param Hangneigung_Proz Slope in percent
#'@param Exposition_Grad Aspect in degree
#'@param J1  Presence(1) / absence(0) of regeneration I (height: 0m-1.3m)
#'@param J2 Presence(1) / absence(0) of regeneration II (height:1.3m-dbh=10.4cm)
#'@param Betriebsart operation type
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=1 is "timber forest, high forest"
##'@param Wuchsbezirk Growth district,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=13 is "Central Alps"
#'@param Relief Slope position,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=2 is "middle slope"
#''@param Bodentyp Soil type,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=9 is
#'"Heavy-textured Cambisols and Luvisols derived from moraine material,
#' non-calcareous loess, or mudstone"
#'@param Bodenfeuchte Soil moisture,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=3 is "moist"
#'@param Schlagvegetation Presence(1) / absence(0) of clear cut vegetation
#'@param PL Period length in years, default=5 years, 5 years is maximum
#'recommended time step for the update of BAL and CCF
#'@param KGW Percentage attributed to forest in ANFI, default=10, "Plot entirely located in forest"
#'@param Stoch Determines if the model is applied stochastically (1) or deterministically (0).
#'default=stochastic. The option stochastic/deterministic applies to the model of ingrowth potential
#'and the Jugend II model. Tree species is alwasy determined stochastically.
#'@param spec.vec Vector of tree species that are present on a plot. Tree species,
#'coding is according to the Austrian National Forest inventory.
#'\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI},
#'By default Norway spruce and European beech are assumed to be present on the plot.
#'@param spec.EW.vec Vector of tree species that are allowed to grow into the plot. Tree species,
#'coding is according to the Austrian National Forest inventory.
#'\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI},
#'By default Norway spruce and European beech are assumed to grow in. Note that 13 tree species models are
#'available from Ledermann (2002), whereas almost 30 tree species are available in the Austrian National
#'Forest Inventory. Thus models for some species might not be available and
#'some models refer to tree species groups.
#'
#'@examples Einwuchs.P(dg_cm=25, Sum_G=40, sum_G10=38, CCF=200)
#'
#'
Einwuchs.P<-function(dg_cm,  Sum_G, sum_G10, CCF,
                     Seehoehe_m=600, Hangneigung_Proz=50, Exposition_Grad=180,
                     J1=0,  J2=0,  Betriebsart=1, Wuchsbezirk=13, Relief=2, Bodentyp=9,
                     Bodenfeuchte=3, Schlagvegetation=0,
                     PL=5,  KGW=10, Stoch=1, spec.vec=c(1,10), spec.EW.vec=c(1,10)){

  #Bestimmen des Einwuchspotentials
  if(Sum_G>0){
    J2<-JugendII.P(dg_cm, Sum_G,  CCF,  Seehoehe_m, Hangneigung_Proz,
                         J1,  J2,  Betriebsart, Wuchsbezirk,  Bodentyp,
                         Schlagvegetation,  PL,  KGW, Stoch)
    if(J2 ==1 & J1 ==1) J1 <- 0
    EWJa<-EWP.P(dg_cm,  Sum_G,  CCF,  Seehoehe_m, Hangneigung_Proz,
                    J1,  J2,  Betriebsart, Wuchsbezirk,  Bodentyp,
                    Schlagvegetation,  PL,  KGW, Stoch)
  } #end if sum_G
  if(Sum_G==0){
    EWJa<-EWP.P(dg_cm,  Sum_G,  CCF,  Seehoehe_m, Hangneigung_Proz,
                    J1,  J2,  Betriebsart, Wuchsbezirk,  Bodentyp,
                    Schlagvegetation,  PL,  KGW, Stoch)
  }

  #Wenn Einwuchspotential gegeben, Anzahl der Einwuchsbäume
  if(EWJa==1){

    nEinwuchs <- EW.Anzahl.P(dg_cm,  Sum_G,  CCF,  Seehoehe_m, Hangneigung_Proz,
                             Exposition_Grad,  J1,  J2, Betriebsart,
                              Wuchsbezirk,  PL,  KGW)
    #Declare empty array for trees
    Baum.mat<-array(0,dim=c(nEinwuchs,3))
    colnames(Baum.mat)<-c("BaumartEW", "BHDEW", "HoeheEW")
    for(i in 1:nEinwuchs){
      WS.vec<-array(0,dim=length(spec.EW.vec))
      for(j in 1:length(spec.EW.vec)){
      WS.vec[j]<-EinwuchsBA.P(spec.EW.vec[j], Sum_G, CCF, Seehoehe_m, Hangneigung_Proz, Exposition_Grad,
                             Bodenfeuchte, Wuchsbezirk, Bodentyp, Betriebsart,
                             spec.vec)
      }#end j
      #Baumart bestimmen, wenn alle Wahrscheinlichkeiten berechnet wurden
        WS.sum<-sum(WS.vec)
        ZZ<-runif(min = 0, max=WS.sum, 1)
        WS.cum<-cumsum(WS.vec)
        Pos<-which.max(WS.cum > ZZ) #Wo wird Zufallszahl erstmals überschritten
        Baumart.EW<-spec.EW.vec[Pos]
      #BHD berechnen
        BHD.EW<-EW.BHD.P(Anzahl_Einwuchs=nEinwuchs,  Sum_G)
      #Höhe berechnen
        Hoehe.EW<-EWH.P(BART_EW=Baumart.EW,  BHD_EW_cm=BHD.EW,  Seehoehe_m,  dg_cm,
                               Sum_G,  sum_G10,  CCF,  Relief)
      #Baumvektor
        Baum.mat[i,]<-cbind(Baumart.EW,BHD.EW,Hoehe.EW)
    }#end i

  }#end if Einwuchspotential

  if(exists("Baum.mat")==TRUE){
    return(Baum.mat)
  }
  if(exists("Baum.mat")==FALSE){
    return("0")
  }

}#end function


