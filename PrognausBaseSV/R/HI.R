#############################################################
#Höhenzuwachs Prognaus - Nachtmann
############################################################
#'@title Height increment model Prognaus
#'
#'@description \code{ih.P} calculates the height increment in m/period
#'
#'@details
#'Calculates the height increment of Prognaus. The model is an Evolon-model
#'(Mende and Albrecht, 2001). The model is published by Nachtmann (2005).
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
#'@param H_m Tree height in m
#'@param Ho_m Dominant height in m
#'@param BAL Basal area of larger trees (Wykoff, 1990)
#'@param CCF Crown competition factor (Krajicek, 1961)
#'@param Seehoehe_m Elevation in m (Range: 200-2200 m)
#'@param Hangneigung_Proz Slope in percent
#'@param Exposition_Grad Aspect in degree
#'@param Bodentiefe_cm Soil depth in cm, model uses dummy variable which is 1 for soildepth <=30cm
#'@param Bodenfeuchte Soil moisture,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI},
#'default=3 is "moist"
#'@param Relief Slope position,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI},
#'default=2 is "middle slope"
#'@param Bodentyp Soil type,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}, default=9 is
#'"Heavy-textured Cambisols and Luvisols derived from moraine material,
#'non-calcareous loess, or mudstone"
#'@param Wuchsbezirk Growth district,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI},
#'default
#'@param PL Period length in years, default=5 years, 5 years is maximum
#'recommended time step for the update of BAL and CCF
#'
#'@examples
#'ih.P(BART=1,H_m=30,Ho_m=30,BAL=0,CCF=200)
#'
#'height2012<-30
#'height2015<-height2012+ih.P(BART=1,H_m=30,Ho_m=30,BAL=0,CCF=200,PL=3)

ih.P<-function(BART=1,  H_m,  Ho_m,  BAL,  CCF,  Seehoehe_m=600,
               Hangneigung_Proz=50, Exposition_Grad=180,  Bodentiefe_cm=50,
               Bodenfeuchte=3,  Relief=2,  Bodentyp=9,  Wuchsbezirk=13, PL=5)
  #Es gelten die Codierungen der Oesterreichischen Waldinventur (1981)
  #Berechnet der jaehrlichen Hoehenzuwachs in Meter
{

if (BART==1) { #Fichte
  b0 = 830.3758
  b1 = -1.48641
  b1a = 0
  b2 = -0.002482722
  b3 = 0.08999699
  b5 = -21.52708
  b7 = 27.66197
  b8 = 55.77791
  b9 = 65.91359
  C = 0.000000000122
  k0 = 0
  k1 = 0.1768697
  k2 = -0.0000494
  k3 = 0
  k4 = 0
  k5 = 0.03018899
  k6 = 0.03984067
  k9 = 0.01550065
  l0 = 3.5
  l1 = 0.06769241
  l2 = 0.07338717
  l3 = 0.03828342
  l5 = 0.007590793
  l6 = -0.006656075
  shopt = 6.535413
  dBoden1 <-  ifelse(Bodentyp %in% c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 16, 20, 24, 25),1,0)
  dBoden2 <-  ifelse(Bodentyp %in% c(13, 14, 22, 23),1,0)
  dBoden3 <-  ifelse(Bodentyp %in% c(18, 19, 21, 26),1,0)
  dWuchs1 <-  ifelse(Wuchsbezirk %in% c(4, 5, 8, 16, 17),1,0)
  dWuchs2 <-  ifelse(Wuchsbezirk %in% c(1, 3, 10, 18, 19),1,0)
}

if (BART==2) { #Tanne
  b0 = 576.8744
  b1 = 0
  b1a = -5.865515
  b2 = -0.005925026
  b3 = 0
  b5 = 0
  b7 = -46.06445
  b8=0
  b9=0
  C = 0.0000377
  k0 = 0.4864953
  k1 = 0
  k2 = 0
  k3 = -0.002183014
  k4 = 0
  k5 = 0
  k6 = 0.05096631
  k9 = 0
  l0 = 1.5
  l1 = 0
  l2 = 0.04532396
  l3 = 0
  l5 = 0
  l6 = 0
  shopt = 0
  dBoden1  <-  ifelse(Bodentyp %in% c(0), 1, 0)
  dBoden2  <-  ifelse(Bodentyp %in% c(2, 3, 4, 5, 8, 10, 13, 14, 15, 16, 19, 20, 22, 23, 24, 25, 26),1,0)
  dBoden3  <-  ifelse(Bodentyp %in% c(0), 1, 0)
  dWuchs1  <-  ifelse(Wuchsbezirk %in% c(0), 1, 0)
  dWuchs2  <-  ifelse(Wuchsbezirk %in% c(0), 1, 0)
}

if (BART==3) {#Laerche
  b0 = 827.5023
  b1 = 0
  b1a = -10.055784
  b2 = -0.00494291
  b3 = 0
  b5 = 0
  b7 = -135.91511
  b8 = -113.635
  b9 = -113.24792
  C = 0.000000105
  k0 = 0.4
  k1 = 0
  k2 = 0
  k3 = -0.002518417
  k4 = -0.07928417
  k5 = 0
  k6 = 0
  k9 = 0
  l0 = 2.5
  l1 = 0.0477241
  l2 = 0.03162142
  l3 = 0
  l5 = 0
  l6 = 0
  shopt = 0
  dBoden1  <-  ifelse(Bodentyp %in% c(6, 10, 12, 13),1,0)
  dBoden2  <-  ifelse(Bodentyp %in% c(2, 3, 4, 9),1,0)
  dBoden3  <-  ifelse(Bodentyp %in% c(0), 1, 0)
  dWuchs1  <-  ifelse(Wuchsbezirk %in% c(0), 1, 0)
  dWuchs2  <-  ifelse(Wuchsbezirk %in% c(0), 1, 0)
}

if (BART==4) {#Weiszkiefer
  b0 = 482.4536
  b1 = 0
  b1a = 0
  b2 = -0.013846154
  b3 = 0
  b5 = 0
  b7 = 0
  b8 = 0
  b9 = 0
  C = 0.000000231
  k0 = 0.4
  k1 = 0
  k2 = 0
  k3 = -0.001940466
  k4 = 0
  k5 = 0
  k6 = 0
  k9 = 0
  l0 = 2.5
  l1 = -0.15991975
  l2 = -0.06091851
  l3 = -0.05142054
  l5 = 0.024703279
  l6 = 0
  shopt = 0
  dBoden1  <-  ifelse(Bodentyp %in% c(11, 17), 1, 0)
  dBoden2  <-  ifelse(Bodentyp %in% c(18), 1, 0)
  dBoden3  <-  ifelse(Bodentyp %in% c(6, 19), 1, 0)
  dWuchs1  <-  ifelse(Wuchsbezirk %in% c(6, 7, 8, 13, 15, 16, 17, 20, 21),1,0)
  dWuchs2  <-  ifelse(Wuchsbezirk %in% c(0), 1, 0)
}

if (BART %in% c(5:9)) {#Sonstiges Nadelholz: Baumarten 5-9
  b0 = 569.1469
  b1 = 0
  b1a = 0
  b2 = -0.012921057
  b3 = 0
  b5 = 0
  b7 = 0
  b8 = 47.12252
  b9=0
  C = 0.0000002272
  k0 = 0.4
  k1 = 0
  k2 = 0
  k3 = -0.002397636
  k4 = 0
  k5 = 0
  k6 = 0
  k9 = 0
  l0 = 2.5
  l1 = 0
  l2 = 0
  l3 = 0
  l5 = 0
  l6 = 0
  shopt = 0
  dBoden1  <-  ifelse(Bodentyp %in% c(0), 1, 0)
  dBoden2  <-  ifelse(Bodentyp %in% c(0), 1, 0)
  dBoden3  <-  ifelse(Bodentyp %in% c(0), 1, 0)
  dWuchs1  <-  ifelse(Wuchsbezirk %in% c(0), 1, 0)
  dWuchs2  <-  ifelse(Wuchsbezirk %in% c(0), 1, 0)
}

#Baumartenspezifische Werte Sonst.NH
if(BART==5) { #Schwarzkiefer
  b0 <-  b0 - 194.7221
}
if(BART==6) {#Zirbe
  C <-  C  - 0.000000186
}
#Strobe==7: keine Anpassung
if(BART==8) {#Douglasie
  b0 <-  b0   - 152.1846
}
if(BART==9) {#Douglasie
  C <- C   - 0.000000168
}

if (BART==10) {#Buche
  b0 = 645.2093
  b1 = 0
  b1a = 0
  b2 = -0.016237986
  b3 = 0
  b5 = 0
  b7 = -57.34682
  b8 = 0
  b9 = 0
  C = 0.000000126
  k0 = 0.4
  k1 = 0
  k2 = 0
  k3 = -0.003353847
  k4 = 0
  k5 = 0
  k6 = 0
  k9 = 0
  l0 = 2.5
  l1 = -0.02554873
  l2 = 0
  l3 = 0
  l5 = 0.036714137
  l6 = 0
  shopt = 0
  dBoden1  <-  ifelse(Bodentyp %in% c(2, 3, 4, 5, 6, 7, 8, 10, 11, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25),1,0)
  dBoden2  <-  ifelse(Bodentyp %in% c(0), 1, 0)
  dBoden3  <-  ifelse(Bodentyp %in% c(0), 1, 0)
  dWuchs1  <-  ifelse(Wuchsbezirk %in% c(3, 4, 5, 21),1,0)
  dWuchs2  <-  ifelse(Wuchsbezirk %in% c(0), 1, 0)
}

if (BART %in% c(11:18)){#Hartlaubbaumarten: Baumarten 11-18;
  b0 = 639.2017
  b1 = 0
  b1a = 0
  b2 = -0.01108105
  b3 = 0
  b5 = 0
  b7 = -32.03469
  b8 = 0
  b9 = 0
  C = 0.0000000000709
  k0 = 0.4
  k1 = 0
  k2 = 0
  k3 = -0.002119498
  k4= 0
  k5 = 0
  k6 = 0
  k9 = 0
  l0 = 3.637734
  l1 = 0
  l2 = 0
  l3 = 0
  l5 = 0
  l6 = 0
  shopt = 0
  dBoden1  <-  ifelse(Bodentyp %in% c(0), 1, 0)
  dBoden2  <-  ifelse(Bodentyp %in% c(10,15,17), 1, 0)
  dBoden3  <-  ifelse(Bodentyp %in% c(0), 1, 0)
  dWuchs1  <-  ifelse(Wuchsbezirk %in% c(0), 1, 0)
  dWuchs2  <-  ifelse(Wuchsbezirk %in% c(0), 1, 0)
}

#Baumartenspezifische Werte Hartlaubbaumarten
if(BART==11) { #Eiche
  l2 <-  0.0454656
}
#12:Hainbuche: keine Anpassung
if(BART==13) {#Esche
  b0 <- b0 + 62.57866
  b9 = 36.31986
}
if(BART==14) {#Ahorn
  b0 <- b0 + 38.4246
}
#15:Ulme: keine Anpassung
if(BART==16) {#Edelkastanie
  b0 <- b0 + 59.97233
}
#17 Robinie, 18 sonst. HLH keine Anpassung

if (BART %in% c(20:28)){#Weichlaubbaumarten: Baumarten: 20-28;
  b0 = 658.5414
  b1 = 0
  b1a = -5.765316
  b2 = 0
  b3 = 0
  b5 = 0
  b7 = 0
  b8 = 0
  b9 = 0
  C = 0.00000000203
  k0 = 0.4
  k1 = 0
  k2 = 0
  k3 = -0.001424609
  k4 = 0
  k5 = -0.06449457
  k6 = 0
  k9 = 0
  l0 = 3.12
  l1 = 0
  l2 = 0
  l3 = 0
  l5 = 0
  l6 = 0
  shopt = 0
  dBoden1  <-  ifelse(Bodentyp %in% c(0), 1, 0)
  dBoden2  <-  ifelse(Bodentyp %in% c(0), 1, 0)
  dBoden3  <-  ifelse(Bodentyp %in% c(0), 1, 0)
  dWuchs1  <-  ifelse(Wuchsbezirk %in% c(0), 1, 0)
  dWuchs2  <-  ifelse(Wuchsbezirk %in% c(0), 1, 0)
}

#Baumartenspezifische Werte Weichlaubbaumarten
if(BART==20) {#Birke
  l0 <-  l0 + 0.0512
}
#BART = 21 Schwarzerle, BART=22 Weißerle BART=23 Linde keine Anpassung
if(BART==24) {#Aspe
  k0 <-  k0 + 0.09482742
}
if(BART==25) {#Schwarzpappel
  b0 <-  b0 + 151.4617
}
if(BART==26) {#Hybridpappel
  b0 <-  b0 + 109.9534
}
#Baumart 27 Baumweide, Baumart 28 sonst. Weichlaubholz keine Anpassung

H_dm  <-  H_m * 10 #Umrechnung in dm
Ho_dm  <-  Ho_m * 10  #Oberhöhe in dm
H_Ho  <-  H_dm / Ho_dm #Verhaeltnis von Hoehe zur Oberhoehe
Seehoehe  <-  Seehoehe_m / 100 #Umrechnung in hm
Expo_rad  <-  (Exposition_Grad / 180) * pi #Umrechnung in Radiant

#Gründigkeit und Wasserhaushaltsklassen
SGR<-ifelse(Bodentiefe_cm <=30, 1, 0) #seichtgruendig

TR<-ifelse(Bodenfeuchte==1,1,0) #trocken
MFR<-ifelse(Bodenfeuchte==2,1,0) #mäßig frisch
FR<-ifelse(Bodenfeuchte==3,1,0) #frisch
SFR<-ifelse(Bodenfeuchte==4,1,0) #sehr frisch, hangsickerfeucht
#Alle Dummyv=0 feucht

#Relief
OH<-ifelse(Relief==1,1,0)  #Oberhang
MH<-ifelse(Relief==2,1,0) #Mittelhang
UH<-ifelse(Relief==3,1,0) #Unterhang
EB<-ifelse(Relief==6,1,0) #Ebene
#Alle Dummyv=0 Mulde

#Evolon-Modell
Kappa = k0 + k1 * H_Ho + k2 * CCF + k3 * BAL + k4 * OH + k5 * MH + k6 * UH + k9 * EB
Lambda = l0 + l1 * dBoden1 + l2 * dBoden2 + l3 *dBoden3+l5 * dWuchs1 + l6 * dWuchs2
b = b0 + b1 * (Seehoehe - shopt) ^ 2 + b1a * Seehoehe + b2 * Hangneigung_Proz ^ 2 + b3 * Hangneigung_Proz * sin(Expo_rad)+
b5 * SGR + b7 * MFR + b8 * FR + b9 * SFR

if(b < H_dm){
  b <- H_dm
}

#jährliche Höhenzuwachs
ihpa = C * H_dm ^ (Kappa) * (b - H_dm) ^ (Lambda)
ih = ihpa / 10 #jährlicher Höhenzuwachs in Meter
ih=ih*PL

}

ih.P<-Vectorize(ih.P)
