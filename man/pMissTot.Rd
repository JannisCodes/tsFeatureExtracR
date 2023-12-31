% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pMissTot.R
\name{pMissTot}
\alias{pMissTot}
\title{Calculate Total Percentage of Missing Values}
\usage{
pMissTot(x)
}
\arguments{
\item{x}{A data frame in which to calculate the percentage of missing values.}
}
\value{
A numeric value representing the total percentage of missing values in the data frame.
}
\description{
This function calculates the total percentage of missing values in a given data frame.
}
\examples{
\dontrun{
data <- data.frame(a = c(1, 2, NA), b = c(4, NA, NA))
missing_percentage <- pMissTot(data)
}
}
