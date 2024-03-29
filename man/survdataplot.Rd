% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{survdataplot}
\alias{survdataplot}
\title{Plot survival data}
\usage{
survdataplot(survivaldat = survframe, colours = colour)
}
\arguments{
\item{survivaldat}{A table with treatment, survival time and binary survival
indicator (1 = death, 0 = alive). as generated by survdata.}

\item{colours}{A named vector with hex colours for each treatment variable
as generated by colourpick.}
}
\value{
a ggplot object.
}
\description{
This function plots the data generated by survdata using survminer, a
ggplot extension.
}
