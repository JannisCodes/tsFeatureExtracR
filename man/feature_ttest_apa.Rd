% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature_ttest_apa.R
\name{feature_ttest_apa}
\alias{feature_ttest_apa}
\title{Format t-test results in APA style}
\usage{
feature_ttest_apa(results_df, concept_in, feature_in, language)
}
\arguments{
\item{results_df}{A data frame containing the results with columns: 'concept', 'feature', 'difference', 't_value', 'df', 'p_value', 'lwr', and 'upr'.}

\item{concept_in}{Concept of interest from the 'concept' column.}

\item{feature_in}{Feature of interest from the 'feature' column.}

\item{language}{The desired output format: 'latex', 'html', or other for plain text.}
}
\value{
A string formatted in APA style with the desired output format.
}
\description{
This function extracts and formats t-test results from a data frame into APA style with options for LaTeX, HTML, or plain text output.
}
\examples{
results <- data.frame(
  concept = c("InteractionContextvoluntary", "AnotherConcept"),
  feature = c("median", "median"),
  difference = c(0.5, 0.7),
  t_value = c(2.5, 3.1),
  df = c(30, 28),
  p_value = c(0.015, 0.002),
  lwr = c(0.2, 0.3),
  upr = c(0.8, 1.1)
)
feature_ttest_apa(results, "InteractionContextvoluntary", "median", "latex")

}
