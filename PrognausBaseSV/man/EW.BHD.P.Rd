% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Einwuchs.R
\name{EW.BHD.P}
\alias{EW.BHD.P}
\title{Diameter of ingrowing trees Prognaus}
\usage{
EW.BHD.P(Anzahl_Einwuchs, Sum_G)
}
\arguments{
\item{Anzahl_Einwuchs}{Number of ingrowing trees calculated by \code{\link{EW.Anzahl.P}}.}

\item{sum_G}{Basal area m²/ha}
}
\description{
\code{EW.BHD.P} calculates the diameter of ingrowing trees
}
\details{
Calculates the diameter for ingrowing trees
The model is part ot the modelling system
for calculating ingrowth on plots developed by Ledermann (2002).
For full references see
\url{https://homepage.boku.ac.at/sonja/Lit_Prog.pdf}.
}
\examples{
EW.BHD.P(Anzahl_Einwuchs=5,  Sum_G40)
}
