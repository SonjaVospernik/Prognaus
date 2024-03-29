% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Biomasse.R
\name{Biomasse}
\alias{Biomasse}
\title{Biomass functions}
\usage{
Biomasse(
  BART = 1,
  BHD_cm,
  H_m = 20,
  CR_Ant = 0.5,
  Typ = "Ast",
  Modell = 1,
  Nadelv = 0,
  Korr = T
)
}
\arguments{
\item{BART}{Tree species,
coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI},
default=1 Norway spruce}

\item{BHD_cm}{dbh in cm}

\item{H_m}{Tree height in m}

\item{CR_Ant}{Crown ratio as ratio}

\item{Typ}{Type of biomass "Ast"=branch biomass, "Nadel"=needle biomass; For broadleaved species
there is no needle or leaf biomass. Default is branch biomass}

\item{Modell}{Type of biomass model, 1-dbh, 2-dbh, height, 3-dbh, height, ln(1-crown ratio)
4-dbh, height, (1-crown ratio). Default is the model based on dbh only.}

\item{Nadelv}{Dummy variable for needle defoliation, 0-no defoliation, 1-defoliated}

\item{Korr}{Bias correction for logarithmic transformation bias, TRUE=correction, FALSE=no correction}
}
\description{
\code{Biomasse} calculates the branch and needle biomass in kg/tree
}
\details{
Calculates the branch and needle biomass. The model for spruce and pine
needle and branch biomass is taken from Eckmüllner (2006).
Models for deciduous trees are taken from
Gschwandtner and Schadauer (2006).
For full references see
\url{https://homepage.boku.ac.at/sonja/Lit_Prog.pdf}.
Coding of tree species Austrian National Forest Inventory
(\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI}).
}
\examples{
Biomasse(BART=1,BHD_cm=30)
}
