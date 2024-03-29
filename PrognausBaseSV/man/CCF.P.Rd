% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Comp.R
\name{CCF.P}
\alias{CCF.P}
\title{Crown competition factor}
\usage{
CCF.P(ID_Punkt = 1, BART = 1, BHD_cm, nrep = 1, MSE_ja = 0)
}
\arguments{
\item{BART}{Tree species,
coding=\href{https://homepage.boku.ac.at/sonja/ANFI_E.pdf}{ANFI},
default=1 Norway spruce}

\item{BHD_cm}{dbh in cm}

\item{nrep}{represented stem number, default=1, data is not from
a sample, i.e. each tree represents 1 tree per hectare}

\item{MSE_ja}{Dummy Variable, default=0, if 1 half of the model MSE is
added to correct for logarithmic transformation bias in the crown diameter
model}

\item{ID}{Unique ID for each sample plot}
}
\description{
\code{CCF.P} calculates the crown competition factor
}
\details{
Calculates the crown competition factor according to Krajicek et al. (1961)
using the open-grown tree diameter functions developed
by Hasenauer (1997) \code{\link{SD.P}}.Circular crown areas are assumed
and multiplied by the represented stem number (e.g. in fixed area plots or
angle count samples)
}
