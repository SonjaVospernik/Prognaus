#'@title Dominant height according to Assmann
#'
#'@description \code{OH.Ass} calculates the dominant height
#'of 100 largest trees per hectare in m
#'@details
#'Calculates the dominant height according to Assmann as Lorey's mean
#'height, dominant height is calculated independent of the tree species.
#'Missing dominant heights are imputed using the quotients between tree
#'species from Vospernik (2000).
#'Function not suitable for full cruise!!!!
#'@param ID Unique ID for each sample plot
#'@param BART Tree species,
#'coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI},
#'default=1 Norway spruce
#'@param BHD_cm dbh in cm
#'@param nrep represented stem number, default=1, data is not from
#'a sample, i.e. each tree represents 1 tree per hectare

OH.Ass<-function(ID_Punkt=1, BART=1, BHD_cm, Hoehe_m, nrep){

  dat<-data.frame("ID_Punkt"=ID_Punkt, "BART"=BART, "BHD"=BHD_cm,
                  "Hoehe"=Hoehe_m,"nrep"=nrep)
  temp<-dat

  ###################################################################
  #Auswahl der Oberhöhenstämme
  ###################################################################

  #Nach Probefläche und BHD sortieren
  temp<-temp[order(temp$ID_Punkt, -temp$BHD),]
  temp$n_cum<-ave(temp$nrep,by=list(temp$ID_Punkt),
                  FUN=cumsum) #Kumulative Summe von n
  temp$OHja<-ifelse(temp$n_cum<=100,1,0) #Alle wo kumulative Summe < 100
  temp$nrepOH<-temp$OHja*temp$nrep #Nrep für alle, wo kumulative Summe < 100

  #nrep der letzten Klasse berechnen
  for(i in 2:nrow(temp)){
      if(temp$OHja[i-1]==1 & temp$OHja[i]==0){
        temp$nrepOH[i]<-100-temp$n_cum[i-1]
      }
  }

  temp1<-subset(temp, nrepOH>0, select=c("ID_Punkt", "BART",
                                          "BHD", "Hoehe", "nrepOH"))

  #################################################################
  #Berechnung der Oberhöhe als Lorey'sche Mittelhöhe der 100
  #stärksten Stämme
  #################################################################

  temp1$n_g<-(temp1$BHD/100)^2*pi/4*temp1$nrepOH #n*g
  temp1$n_g_h<-temp1$n_g*temp1$Hoehe #n*g*h

  sumNG<-aggregate(temp1$n_g, by=list(temp1$ID_Punkt,temp1$BART), FUN=sum)
  sumNGH<-aggregate(temp1$n_g_h, by=list(temp1$ID_Punkt,temp1$BART), FUN=sum)

  colnames(sumNG)<-c("ID_Punkt","BART","sumNG")
  colnames(sumNGH)<-c("ID_Punkt","BART","sumNGH")

  tempOH<-merge(sumNG, sumNGH, by.x=c("ID_Punkt","BART"),
                by.y=c("ID_Punkt","BART"))
  tempOH$OH<-tempOH$sumNGH/tempOH$sumNG
  tempOH$ID_Punkt<-as.numeric(tempOH$ID_Punkt)

  #####################################################################
  #Ergänzung fehlender Oberhöhen
  ####################################################################

  #Alle im Datensatz vorkommenden Probeflächen-Baumartenkombinationen
  temp3<-unique(subset(dat, select=c("ID_Punkt", "BART")))

  tempOH1<-merge(temp3,
                 tempOH[,c("ID_Punkt","BART","OH")],
                 by.x=c("ID_Punkt","BART"),
                 by.y=c("ID_Punkt","BART"),
                 all.x=TRUE)

  tempOH1$OH_ja<-ifelse(is.na(tempOH1$OH),0,1)

  tempOH1<-tempOH1[order(tempOH1$ID_Punkt,-tempOH1$OH_ja, -tempOH1$BART),]

  for(i in 1:nrow(tempOH1)){

    if(!is.na(tempOH1$OH[i])){#Wenn Oberhöhe vorhanden an temp Variable zuweisen
      tempBA<-tempOH1$BART[i]
      tempOH<-tempOH1$OH[i]
    }

    if(is.na(tempOH1$OH[i])){#Wenn Oberhöhe fehlt, aus tempVariablen und
      #OH Funktionen zuweisen

    if(tempBA==1) CODE<-'A'
    if(tempBA==2) CODE<-'B'
    if(tempBA==3) CODE<-'C'
    if(tempBA %in% c(4:6)) CODE<-'D'
    if(tempBA==10 | tempBA>11) CODE<-'E'
    if(tempBA==11) CODE<-'F'

    switch(CODE,
           A={tempOH1$OH[i]<-OHQFi(tempOH, tempOH1$BART[i])
           },
           B={tempOH1$OH[i]<-OHQTa(tempOH, tempOH1$BART[i])
           },
           C={tempOH1$OH[i]<-OHQLa(tempOH, tempOH1$BART[i])
           },
           D={tempOH1$OH[i]<-OHQKi(tempOH, tempOH1$BART[i])
           },
           E={tempOH1$OH[i]<-OHQBu(tempOH, tempOH1$BART[i])
           },
           F={tempOH1$OH[i]<-OHQEi(tempOH, tempOH1$BART[i])
           }
    )#End switch
  } #End if
}#End for

  ##################################################################
  #Oberhöhen an Originaldatensatz anbinden
  ##################################################################
  dat1<-merge(dat, tempOH1[,c("ID_Punkt", "BART", "OH")],
              by.x=c("ID_Punkt", "BART"),
              by.y=c("ID_Punkt", "BART"), all.x=TRUE, sort=FALSE)


  OH<-dat1$OH
}#Ende Oberhöhenberechnung


#Berechnet die Oberhöhe einer anderen Baumart aus der Oberhöhe von Fichte
OHQFi<-function(OH_m, BART, Seehoehe_m=NA, Hangneigung_Proz=NA,
                  Bodenfeuchte=NA,Gha=NA) {

  Seehoehe=Seehoehe_m/100

  if(BART==1) CODE<-'A'
  if(BART==2) CODE<-'B'
  if(BART==3) CODE<-'C'
  if(BART %in% c(4:6)) CODE<-'D'
  if(BART==10 | BART>11) CODE<-'E'
  if(BART==11) CODE<-'F'

  switch(CODE,
         #Fichte
         A={OH_neu<-OH_m*1
         },
         #Tanne
         B={OH_neu<-OH_m*0.95
         },
         #Lärche
         C={OH_neu<-OH_m*1.02
            # if(!is.na(Seehoehe) & !is.na(Bodenfeuchte)){
            #   D_Bf<-ifelse(Bodenfeuchte %in% c(1:3),0,1)
            #   OH_neu<-OH_m*(1.212-0.00620*OH_m-0.00024*Seehoehe^2-0.05800*D_Bf)
            # }
          },
         #Kiefer,Schwarzkiefer,Zirbe
         D={OH_neu<-OH_m*0.98
            # if(!is.na(Seehoehe) & !is.na(Gha)){
            #   OH_neu<-OH_m*(1.119-0.00730*OH_m-0.00720*Seehoehe-0.00099*Gha)
            # }
         },
         #Buche,sonstiges Laubholz
         E={OH_neu<-OH_m*0.91
          # if(!is.na(Seehoehe) & !is.na(Hangneigung_Proz)){
          #   OH_neu<-OH_m*(1.166-0.00550*OH_m-0.01100*Seehoehe-0.000008*Hangneigung_Proz^2)
          # }
        },
         #Eiche
         F={OH_neu<-OH_m*0.87
         }
  )

}

#Berechnet die Oberhöhe einer anderen Baumart aus der Oberhöhe von Tanne
OHQTa<-function(OH_m, BART, Seehoehe_m=NA, Hangneigung_Proz=NA,
                Bodenfeuchte=NA,Gha=NA) {

  Seehoehe=Seehoehe_m/100

  if(BART==1) CODE<-'A'
  if(BART==2) CODE<-'B'
  if(BART==3) CODE<-'C'
  if(BART %in% c(4:6)) CODE<-'D'
  if(BART==10 | BART>11) CODE<-'E'
  if(BART==11) CODE<-'F'

  switch(CODE,
         #Fichte
         A={OH_neu<-OH_m*1.05
         },
         #Tanne
         B={OH_neu<-OH_m*1.00
         },
         #Lärche
         C={OH_neu<-OH_m*1.02*1.05
         # if(!is.na(Seehoehe) & !is.na(Bodenfeuchte)){
         #   D_Bf<-ifelse(Bodenfeuchte %in% c(1:3),0,1)
         #   OH_neu<-1.05*OH_m*(1.212-0.00620*OH_m-0.00024*Seehoehe^2-0.05800*D_Bf)
         # }
        },
         #Kiefer,Schwarzkiefer,Zirbe
         D={OH_neu<-1.05*OH_m*0.98
         # if(!is.na(Seehoehe) & !is.na(Gha)){
         #   OH_neu<-1.05*OH_m*(1.119-0.00730*OH_m-0.00720*Seehoehe-0.00099*Gha)
         # }
         },
         #Buche,sonstiges Laubholz
         E={OH_neu<-1.05*OH_m*0.91
         # if(!is.na(Seehoehe) & !is.na(Hangneigung_Proz)){
         #  OH_neu<-1.05*OH_m*(1.166-0.00550*OH_m-0.01100*Seehoehe-0.000008*Hangneigung_Proz^2)
         #  }
         },
         #Eiche
         F={OH_neu<-1.05*OH_m*0.87
         }
  )

}

#Berechnet die Oberhöhe einer anderen Baumart aus der Oberhöhe von Lärche
OHQLa<-function(OH_m, BART, Seehoehe_m=600, Hangneigung_Proz=50) {

  Seehoehe=Seehoehe_m/100

  if(BART==1) CODE<-'A'
  if(BART==2) CODE<-'B'
  if(BART==3) CODE<-'C'
  if(BART %in% c(4:6)) CODE<-'D'
  if(BART==10 | BART>11) CODE<-'E'
  if(BART==11) CODE<-'F'

  switch(CODE,
         #Fichte
         A={OH_neu<-OH_m*0.98
         },
         #Tanne
         B={OH_neu<-OH_m*0.92
         },
         #Lärche
         C={OH_neu<-OH_m*1
         },
         #Kiefer,Schwarzkiefer,Zirbe
         D={OH_neu<-OH_m*0.89
         },
         #Buche,sonstiges Laubholz
         E={OH_neu<-OH_m*0.92
         # if(!is.na(Seehoehe) & !is.na(Hangneigung_Proz)){
         #   OH_neu<-OH_m*(0.961-0.00180*Seehoehe^2-0.0000249*Hangneigung_Proz^2)
         #  }
         },
         #Eiche
         F={OH_neu<-OH_m*0.92
         }
  )

}

#Berechnet die Oberhöhe einer anderen Baumart aus der Oberhöhe von Kiefer,
#Schwarzkiefer oder Zirbe
OHQKi<-function(OH_m, BART) {

  if(BART==1) CODE<-'A'
  if(BART==2) CODE<-'B'
  if(BART==3) CODE<-'C'
  if(BART %in% c(4:6)) CODE<-'D'
  if(BART==10 | BART>11) CODE<-'E'
  if(BART==11) CODE<-'F'

  switch(CODE,
         #Fichte
         A={OH_neu<-OH_m*1.02
         },
         #Tanne
         B={OH_neu<-OH_m*0.97
         },
         #Lärche
         C={OH_neu<-OH_m*1.12
         },
         #Kiefer aus anderer Kiefer
         D={OH_neu<-OH_m*1.00
         },
         #Buche,sonstiges Laubholz
         E={OH_neu<-OH_m*0.98
         },
         #Eiche
         F={OH_neu<-OH_m*1.01
         }

  )

}

#Berechnet die Oberhöhe einer anderen Baumart aus der Oberhöhe von Buche
#oder sonstigem Laubholz
OHQBu<-function(OH_m, BART) {

  if(BART==1) CODE<-'A'
  if(BART==2) CODE<-'B'
  if(BART==3) CODE<-'C'
  if(BART %in% c(4:6)) CODE<-'D'
  if(BART==10 | BART>11) CODE<-'E'
  if(BART==11) CODE<-'F'

  switch(CODE,
         #Fichte
         A={OH_neu<-OH_m*1.10
         },
         #Tanne
         B={OH_neu<-OH_m*1.04
         },
         #Lärche
         C={OH_neu<-OH_m*1.09
         },
         #Kiefer
         D={OH_neu<-OH_m*1.025
         },
         #Buche,sonstiges Laubholz
         E={OH_neu<-OH_m*1
         },
         #Eiche
         F={OH_neu<-OH_m*0.98
         }
  )

}


#Berechnet die Oberhöhe einer anderen Baumart aus der Oberhöhe von Eiche
OHQEi<-function(OH_m, BART) {

  if(BART==1) CODE<-'A'
  if(BART==2) CODE<-'B'
  if(BART==3) CODE<-'C'
  if(BART %in% c(4:6)) CODE<-'D'
  if(BART==10 | BART>11) CODE<-'E'
  if(BART==11) CODE<-'F'

  switch(CODE,
         #Fichte
         A={OH_neu<-OH_m*1.14
         },
         #Tanne
         B={OH_neu<-OH_m*1.08
         },
         #Lärche
         C={OH_neu<-OH_m*1.09
         },
         #Kiefer,Schwarzkiefer,Zirbe
         C={OH_neu<-OH_m*0.99
         },
         #Buche,sonstiges Laubholz
         E={OH_neu<-OH_m*1.02
         },
         #Eiche
         F={OH_neu<-OH_m*1.00
         }

  )

}


