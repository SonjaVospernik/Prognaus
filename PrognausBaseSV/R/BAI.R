###########################################################################
#Kreisflächenzuwachs Prognaus
###########################################################################
#'@title Basal area increment model Prognaus
#'
#'@description \code{BAI.P} calculates the basal area increment in cm²/period
#'
#'@details
#'Calculates the basal area increment of Prognaus. The model is based
#'on Monserud and Sterba (1996); Coefficients are taken from Hasenauer (2000).
#'For full references see
#'\url{https://homepage.boku.ac.at/sonja/Lit_Prog.pdf}.
#'Coding of tree species, soil moisture, relief, soil type, vegetation type
#'and growth district uses the coding of the
#'Austrian National Forest Inventory
#'(\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}).
#'
#'@param BART Tree species,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI},
#'default=1 Norway spruce
#'@param BHD_cm dbh in cm
#'@param CR_Ant Crown ratio as ratio
#'@param BAL Basal area of larger trees (Wykoff, 1990)
#'@param CCF Crown competition factor (Krajicek, 1961)
#'@param Seehoehe_m Elevation in m (Range: 200-2200 m)
#'@param Hangneigung_Proz Slope in percent
#'@param Exposition_Grad Aspect in degree
#'@param HF_cm Thickness of F-humus layer in cm
#'@param HH_cm Thickness of H-humus layer in cm
#'@param Bodentiefe_cm Soil depth in cm, model uses dummy variable which is 1 for soildepth <=30cm
#'@param Bodenfeuchte Soil moisture,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=3 is "moist"
#'@param Relief Slope position,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=2 is "middle slope"
#'@param Bodentyp Soil type,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=9 is
#' "Heavy-textured Cambisols and Luvisols derived from moraine material,
#'  non-calcareous loess, or mudstone"
#'@param Vegetationstyp Vegetation type,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=1 is Shade-tolerant herbs types
#'@param Wuchsbezirk Growth district,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=13 is Central Alps
#'@param MSE_ja Dummy variable, default=0, if 1 half of the model MSE is
#'added to correct for logarithmic transformation bias
#'@param PL Period length in years, default=5 years, 5 years is maximum
#'recommended time step for the update of BAL and CCF
#'
#'@examples
#'BAI.P(BART=1,BHD_cm=30,CR_Ant = 0.5,BAL=0,CCF=200)

BAI.P<-function(BART=1, BHD_cm, CR_Ant, BAL, CCF,
                Seehoehe_m=600, Hangneigung_Proz=50, Exposition_Grad=180,
                HF_cm=3, HH_cm=2, Bodentiefe_cm=50,
                Bodenfeuchte=3, Relief=2, Bodentyp=9, Vegetationstyp=1,
                Wuchsbezirk=13, MSE_ja=0, PL=5){

if (BART==1) { #Fichte
  a0 = 0.07
  b1 = 1.396876
  b2 = -0.000165
  b3 = 0.634179
  c1 = -0.017369
  c2 = -0.000497
  d1 = -0.003824
  d1a = 0
  d2 = -0.186184
  d2a = 0
  d3 = 0.034217
  d4 = -0.076437
  d5 = -0.018425
  d6 = -0.009333
  SD = -0.117116
  sm = 0.122647
  SR = 0
  s1 = 0.410682
  s2 = 0.350527
  s3 = 0.174713
  s4 = 0
  v1 = 0.187571
  v2 = 0.323107
  v3 = 0
  g1 = 0.106262
  g2 = 0.043582
  MSE = 0.559
  X = 6
  dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(4, 5, 8, 16, 17), 1, 0)
  dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(1, 3, 10, 18, 19), 1, 0)
  dBodentyp1<-ifelse(Bodentyp %in% c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 16, 20, 24, 25), 1, 0)
  dBodentyp2<-ifelse(Bodentyp %in% c(1, 13, 14, 22, 23), 1, 0)
  dBodentyp3<-ifelse(Bodentyp %in% c(18, 19, 21, 26), 1, 0)
  dBodentyp4<-ifelse(Bodentyp %in% c(0), 1, 0)
  dVegetationstyp1<-ifelse(Vegetationstyp %in% c(1, 4, 12), 1, 0)
  dVegetationstyp2<-ifelse(Vegetationstyp %in% c(18, 19, 20), 1, 0)
  dVegetationstyp3<-ifelse(Vegetationstyp %in% c(0), 1, 0)
  dRelief1<-ifelse(Relief %in% c(0), 1, 0)
  dBodenfeuchte1<-ifelse(Bodenfeuchte %in% c(2, 3, 4), 1, 0)
}

if (BART==2) { #Tanne
  a0 = -0.712624
  b1 = 1.469029
  b2 = -0.000117
  b3 = 0.502449
  c1 = -0.015949
  c2 = -0.000564
  d1 = 0
  d1a = 0
  d2 = 0
  d2a = 0
  d3 = 0
  d4 = 0
  d5 = 0
  d6 = 0
  SD = 0
  sm = 0
  SR = 0.293053
  s1 = 0.550273
  s2 = 0.383876
  s3 = 0
  s4 = 0
  v1 = 0.185379
  v2 = 0
  v3 = 0
  g1 = 0.37513
  g2 = 0.211501
  MSE = 0.753
  X = 0
  dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(17, 19), 1, 0)
  dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 18, 20, 21), 1, 0)
  dBodentyp1<-ifelse(Bodentyp %in% c(6, 7, 9, 11, 12), 1, 0)
  dBodentyp2<-ifelse(Bodentyp %in% c(2, 3, 4, 5, 8, 10, 13, 14, 15, 16, 19, 20, 22, 23, 24, 25, 26), 1, 0)
  dBodentyp3<-ifelse(Bodentyp %in% c(0), 1, 0)
  dBodentyp4<-ifelse(Bodentyp %in% c(0), 1, 0)
  dVegetationstyp1<-ifelse(Vegetationstyp %in% c(4, 6, 8, 18), 1, 0)
  dVegetationstyp2<-ifelse(Vegetationstyp %in% c(0), 1, 0)
  dVegetationstyp3<-ifelse(Vegetationstyp %in% c(0), 1, 0)
  dRelief1<-ifelse(Relief %in% c(3), 1, 0)
  dBodenfeuchte1<-ifelse(Bodenfeuchte %in% c(0), 1, 0)
}

if (BART==3) {   #Lärche
  a0 = 0.696551
  b1 = 1.246003
  b2 = -0.000137
  b3 = 0.433521
  c1 = -0.015379
  c2 = -0.000401
  d1 = 0
  d1a = -0.042101
  d2 = 0
  d2a = 0
  d3 = 0
  d4 = 0
  d5 = 0
  d6 = 0
  SD = 0
  sm = 0
  SR = 0
  s1 = 0.497502
  s2 = 0.181174
  s3 = 0
  s4 = 0
  v1 = 0.537855
  v2 = 0.342462
  v3 = 0.214816
  g1 = 0.199215
  g2 = 0
  MSE = 0.573
  X = 0
  dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(12, 13, 14, 15, 17, 18, 19, 20), 1, 0)
  dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
  dBodentyp1<-ifelse(Bodentyp %in% c(6, 8, 10, 12, 13), 1, 0)
  dBodentyp2<-ifelse(Bodentyp %in% c(2, 3, 4, 9), 1, 0)
  dBodentyp3<-ifelse(Bodentyp %in% c(0), 1, 0)
  dBodentyp4<-ifelse(Bodentyp %in% c(0), 1, 0)
  dVegetationstyp1<-ifelse(Vegetationstyp %in% c(18, 19), 1, 0)
  dVegetationstyp2<-ifelse(Vegetationstyp %in% c(1, 4, 7, 13), 1, 0)
  dVegetationstyp3<-ifelse(Vegetationstyp %in% c(2, 6, 8, 10, 11), 1, 0)
  dRelief1<-ifelse(Relief %in% c(0), 1, 0)
  dBodenfeuchte1<-ifelse(Bodenfeuchte %in% c(0), 1, 0)
}

if (BART==4) {#Kiefer
  a0 = 0.658017
  b1 = 1.287496
  b2 = -0.00024
  b3 = 0.722032
  c1 = -0.011146
  c2 = 0
  d1 = -0.00405
  d1a = 0
  d2 = 0
  d2a = -0.225295
  d3 = 0
  d4 = 0
  d5 = 0
  d6 = -0.041745
  SD = 0
  sm = 0.269704
  SR = 0
  s1 = -0.402267
  s2 = -0.347126
  s3 = -0.170259
  s4 = 0.394765
  v1 = 0.130182
  v2 = 0
  v3 = 0
  g1 = 0
  g2 = 0.10559
  MSE = 0.595
  X = 6
  dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
  dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(6, 7, 8, 13, 15, 16, 17, 20, 21), 1, 0)
  dBodentyp1<-ifelse(Bodentyp %in% c(11, 17), 1, 0)
  dBodentyp2<-ifelse(Bodentyp %in% c(18), 1, 0)
  dBodentyp3<-ifelse(Bodentyp %in% c(6, 19), 1, 0)
  dBodentyp4<-ifelse(Bodentyp %in% c(1, 5, 7, 14, 16, 20, 22), 1, 0)
  dVegetationstyp1<-ifelse(Vegetationstyp %in% c(3, 4, 8, 12, 19), 1, 0)
  dVegetationstyp2<-ifelse(Vegetationstyp %in% c(0), 1, 0)
  dVegetationstyp3<-ifelse(Vegetationstyp %in% c(0), 1, 0)
  dRelief1<-ifelse(Relief %in% c(0), 1, 0)
  dBodenfeuchte1<-ifelse(Bodenfeuchte %in% c(1, 2, 3), 1, 0)
}

if (BART==5) {#Schwarzkiefer
  a0 = -1.54957
  b1 = 1.635013
  b2 = -0.000173
  b3 = 0
  c1 = -0.015162
  c2 = 0
  d1 = -0.007108
  d1a = 0
  d2 = -0.52881
  d2a = 0
  d3 = 0
  d4 = 0
  d5 = 0
  d6 = 0
  SD = 0
  sm = 0
  SR = 0.433283
  s1 = 0
  s2 = 0
  s3 = 0
  s4 = 0
  v1 = 0
  v2 = 0
  v3 = 0
  g1 = 0
  g2 = 0
  MSE = 0.595
  X = 0
  dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
  dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
  dBodentyp1<-ifelse(Bodentyp %in% c(0), 1, 0)
  dBodentyp2<-ifelse(Bodentyp %in% c(0), 1, 0)
  dBodentyp3<-ifelse(Bodentyp %in% c(0), 1, 0)
  dBodentyp4<-ifelse(Bodentyp %in% c(0), 1, 0)
  dVegetationstyp1<-ifelse(Vegetationstyp %in% c(0), 1, 0)
  dVegetationstyp2<-ifelse(Vegetationstyp %in% c(0), 1, 0)
  dVegetationstyp3<-ifelse(Vegetationstyp %in% c(0), 1, 0)
  dRelief1<-ifelse(Relief %in% c(0), 1, 0)
  dBodenfeuchte1<-ifelse(Bodenfeuchte %in% c(0), 1, 0)
}

if (BART==6) {#Zirbe
  a0 = 2.56557
  b1 = 0.466866
  b2 = 0
  b3 = 0
  c1 = -0.010259
  c2 = 0
  d1 = -0.029576
  d1a = 0
  d2 = 0
  d2a = 0
  d3 = 0
  d4 = 0
  d5 = 0
  d6 = 0
  SD = 0
  sm = 0
  SR = 0
  s1 = 0
  s2 = 0
  s3 = 0
  s4 = 0
  v1 = 0
  v2 = 0
  v3 = 0
  g1 = 0.310889
  g2 = 0
  MSE = 0.595
  X = 18
  dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 16, 17, 18, 19, 20, 21), 1, 0)
  dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
  dBodentyp1<-ifelse(Bodentyp %in% c(0), 1, 0)
  dBodentyp2<-ifelse(Bodentyp %in% c(0), 1, 0)
  dBodentyp3<-ifelse(Bodentyp %in% c(0), 1, 0)
  dBodentyp4<-ifelse(Bodentyp %in% c(0), 1, 0)
  dVegetationstyp1<-ifelse(Vegetationstyp %in% c(0), 1, 0)
  dVegetationstyp2<-ifelse(Vegetationstyp %in% c(0), 1, 0)
  dVegetationstyp3<-ifelse(Vegetationstyp %in% c(0), 1, 0)
  dRelief1<-ifelse(Relief %in% c(0), 1, 0)
  dBodenfeuchte1<-ifelse(Bodenfeuchte %in% c(0), 1, 0)
}

if (BART==10) {#Buche
  a0 = -0.644436
  b1 = 1.589274
  b2 = -0.000189
  b3 = 0.630734
  c1 = -0.016353
  c2 = -0.000175
  d1 = -0.00308
  d1a = 0
  d2 = -0.269852
  d2a = 0
  d3 = 0
  d4 = 0
  d5 = -0.031316
  d6 = 0.021224
  SD = -0.215741
  sm = 0
  SR = 0
  s1 = 0.178703
  s2 = 0
  s3 = 0
  s4 = 0
  v1 = 0.710601
  v2 = 0.458712
  v3 = 0
  g1 = 0.329867
  g2 = 0.179966
  MSE = 0.596
  X = 0
  dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(3, 4, 5, 21), 1, 0)
  dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(1, 13, 16, 17, 19, 20), 1, 0)
  dBodentyp1<-ifelse(Bodentyp %in% c(2, 3, 4, 5, 6, 7, 8, 10, 11, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25), 1, 0)
  dBodentyp2<-ifelse(Bodentyp %in% c(0), 1, 0)
  dBodentyp3<-ifelse(Bodentyp %in% c(0), 1, 0)
  dBodentyp4<-ifelse(Bodentyp %in% c(0), 1, 0)
  dVegetationstyp1<-ifelse(Vegetationstyp %in% c(3, 8, 9, 18, 19), 1, 0)
  dVegetationstyp2<-ifelse(Vegetationstyp %in% c(1, 2, 4, 5, 6, 12), 1, 0)
  dVegetationstyp3<-ifelse(Vegetationstyp %in% c(0), 1, 0)
  dRelief1<-ifelse(Relief %in% c(0), 1, 0)
  dBodenfeuchte1<-ifelse(Bodenfeuchte %in% c(0), 1, 0)
}

if (BART==11) {#Eiche
  a0 = -0.252064
  b1 = 1.44802
  b2 = -0.00014
  b3 = 0
  c1 = -0.02681
  c2 = 0
  d1 = 0
  d1a = 0
  d2 = 0
  d2a = 0
  d3 = 0.347115
  d4 = 0.188109
  d5 = 0
  d6 = 0
  SD = 0
  sm = 0
  SR = 0
  s1 = 0.597342
  s2 = 0.307345
  s3 = 0.115069
  s4 = 0
  v1 = 0.26517
  v2 = 0
  v3 = 0
  g1 = 0
  g2 = 0
  MSE = 0.534
  X = 0
  dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
  dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
  dBodentyp1<-ifelse(Bodentyp %in% c(4, 12, 16), 1, 0)
  dBodentyp2<-ifelse(Bodentyp %in% c(10, 15, 17), 1, 0)
  dBodentyp3<-ifelse(Bodentyp %in% c(1, 3, 6, 8, 9, 11, 13, 14), 1, 0)
  dBodentyp4<-ifelse(Bodentyp %in% c(0), 1, 0)
  dVegetationstyp1<-ifelse(Vegetationstyp %in% c(4, 19, 20), 1, 0)
  dVegetationstyp2<-ifelse(Vegetationstyp %in% c(0), 1, 0)
  dVegetationstyp3<-ifelse(Vegetationstyp %in% c(0), 1, 0)
  dRelief1<-ifelse(Relief %in% c(0), 1, 0)
  dBodenfeuchte1<-ifelse(Bodenfeuchte %in% c(0), 1, 0)
}

if (BART==28 | BART==26 | BART==13) {#sonst. Laubholz
  a0 = 0.936249
  b1 = 1.133501
  b2 = 0
  b3 = 0.517598
  c1 = -0.01935
  c2 = 0
  d1 = 0
  d1a = 0
  d2 = -0.460704
  d2a = 0
  d3 = 0.145024
  d4 = -0.040436
  d5 = 0
  d6 = 0
  SD = 0
  sm = 0
  SR = 0
  s1 = 0
  s2 = 0
  s3 = 0
  s4 = 0
  v1 = 0
  v2 = 0
  v3 = 0
  g1 = 0
  g2 = 0
  MSE = 0.757
  X = 0
  dWuchsbezirk1<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
  dWuchsbezirk2<-ifelse(Wuchsbezirk %in% c(0), 1, 0)
  dBodentyp1<-ifelse(Bodentyp %in% c(0), 1, 0)
  dBodentyp2<-ifelse(Bodentyp %in% c(0), 1, 0)
  dBodentyp3<-ifelse(Bodentyp %in% c(0), 1, 0)
  dBodentyp4<-ifelse(Bodentyp %in% c(0), 1, 0)
  dVegetationstyp1<-ifelse(Vegetationstyp %in% c(0), 1, 0)
  dVegetationstyp2<-ifelse(Vegetationstyp %in% c(0), 1, 0)
  dVegetationstyp3<-ifelse(Vegetationstyp %in% c(0), 1, 0)
  dRelief1<-ifelse(Relief %in% c(0), 1, 0)
  dBodenfeuchte1<-ifelse(Bodenfeuchte %in% c(0), 1, 0)
}

if (BART==13){#Esche
  a0=1.28677
}

if (BART==26) {
  a0 = 1.879919
}

Exposition_rad <- (Exposition_Grad / 180) * pi
Bodentiefe1<-ifelse(Bodentiefe_cm <=30, 1, 0)

BAI = exp(a0 + b1 * log(BHD_cm) + b2 * BHD_cm ^ 2 + b3 * log(CR_Ant) + c1 * BAL+
            c2 * CCF + d1 * (Seehoehe_m / 100 - X) ^ 2 +
            d1a * (Seehoehe_m / 100) + d2 * (Hangneigung_Proz / 100) ^ 2+
            d2a * Hangneigung_Proz / 100 + d3 * (Hangneigung_Proz / 100) * sin(Exposition_rad)+
            d4 * (Hangneigung_Proz / 100) * cos(Exposition_rad) + d5 * HF_cm + d6 * HH_cm+
            SD * Bodentiefe1 + sm * dBodenfeuchte1 + SR * dRelief1+
            s1 * dBodentyp1 + s2 * dBodentyp2+
            s3 * dBodentyp3 + s4 * dBodentyp4+
            v1 * dVegetationstyp1 + v2 * dVegetationstyp2+
            v3 * dVegetationstyp3 + g1 * dWuchsbezirk1+
            g2 * dWuchsbezirk2 + 0.5 * MSE * MSE_ja)

BAI<-PL*BAI/5

}

BAI.P<-Vectorize(BAI.P)

##########################################################################
#Durchmesserzuwachs
##########################################################################
#'@title Diameter increment model Prognaus
#'
#'@description \code{id.P} calculates the diameter increment in cm/period
#'
#'@details
#'Calculates the diameter increment of Prognaus. The model calls the
#'basal area increment function of Prognaus and converts it to diameter
#'increment. For details see description of \code{\link{BAI.P}}
#'
#'@examples
#'id.P(BART=1,BHD_cm=30,CR_Ant = 0.5,BAL=0,CCF=200)
#'
#'dbh2012<-30
#'dbh2015<-dbh2012+id.P(BART=1,BHD_cm=30,CR_Ant = 0.5,BAL=0,CCF=200,PL=3)

id.P<-function(BART=1, BHD_cm, CR_Ant, BAL, CCF,
                Seehoehe_m=600, Hangneigung_Proz=50, Exposition_Grad=180,
                HF_cm=3, HH_cm=2, Bodentiefe_cm=50,
                Bodenfeuchte=3, Relief=2, Bodentyp=9, Vegetationstyp=1,
                Wuchsbezirk=13, MSE_ja=0, PL=5){

BAI<-BAI.P(BART, BHD_cm, CR_Ant, BAL, CCF,
           Seehoehe_m, Hangneigung_Proz, Exposition_Grad,
           HF_cm, HH_cm, Bodentiefe_cm,
           Bodenfeuchte, Relief, Bodentyp, Vegetationstyp,
           Wuchsbezirk, MSE_ja, PL)

BHD2_cm<-sqrt(BHD_cm^2+BAI*4/pi)

id<-BHD2_cm-BHD_cm #5-jährige id

}

id.P<-Vectorize(id.P)

