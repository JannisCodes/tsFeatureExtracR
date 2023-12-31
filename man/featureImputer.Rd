% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/featureImputer.R
\name{featureImputer}
\alias{featureImputer}
\title{Feature Imputation Function}
\usage{
featureImputer(extractorList)
}
\arguments{
\item{extractorList}{A list generated by the featureExtractor function, containing raw and standardized features, as well as the GAM models.}
}
\value{
A list similar to the input, but with additional elements:
   * featuresImp: The result of the imputation process, an object of class "mids".
   * featuresImpZ: A data frame of standardized imputed features.
   * featuresImpZMat: A data frame of standardized imputed features with person identifiers as row names.
}
\description{
This function performs imputation on the features data derived from the featureExtractor function.
It uses the Multiple Imputation by Chained Equations (MICE) algorithm to handle missing data.
It then standardizes the imputed data and returns the modified input list with the imputed and standardized data.
}
\examples{
\dontrun{
extractorList <- featureExtractor(df, c("item1", "item2"), "ID", "time")
imputedList <- featureImputer(extractorList)
}
}
