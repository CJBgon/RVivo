% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processing.R
\name{exptime}
\alias{exptime}
\title{measurement intervals}
\usage{
exptime(volumematrix = micematrix, datecolumn = col)
}
\arguments{
\item{volumematrix}{matrix of the format: rows = mice, columns = dates,
fill = tumour volume.}

\item{datecolumn}{Vector of dates of measurements, in case they differ from
the column names.}
}
\value{
A vector of difference in tumour size.
}
\description{
This function calculates the interval in days between measurements
}
