% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BAI.R
\name{id.P}
\alias{id.P}
\title{Diameter increment model Prognaus}
\usage{
id.P(
  BART = 1,
  BHD_cm,
  CR_Ant,
  BAL,
  CCF,
  Seehoehe_m = 600,
  Hangneigung_Proz = 50,
  Exposition_Grad = 180,
  HF_cm = 3,
  HH_cm = 2,
  Bodentiefe_cm = 50,
  Bodenfeuchte = 3,
  Relief = 2,
  Bodentyp = 9,
  Vegetationstyp = 1,
  Wuchsbezirk = 13,
  MSE_ja = 0,
  PL = 5
)
}
\description{
\code{id.P} calculates the diameter increment in cm/period
}
\details{
Calculates the diameter increment of Prognaus. The model calls the
basal area increment function of Prognaus and converts it to diameter
increment. For details see description of \code{\link{BAI.P}}
}
\examples{
id.P(BART=1,BHD_cm=30,CR_Ant = 0.5,BAL=0,CCF=200)

dbh2012<-30
dbh2015<-dbh2012+id.P(BART=1,BHD_cm=30,CR_Ant = 0.5,BAL=0,CCF=200,PL=3)
}
