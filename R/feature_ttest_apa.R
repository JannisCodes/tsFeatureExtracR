#' Format t-test results in APA style
#'
#' This function extracts and formats t-test results from a data frame into APA style with options for LaTeX, HTML, or plain text output.
#'
#' @param results_df A data frame containing the results with columns: 'concept', 'feature', 'difference', 't_value', 'df', 'p_value', 'lwr', and 'upr'.
#' @param concept_in Concept of interest from the 'concept' column.
#' @param feature_in Feature of interest from the 'feature' column.
#' @param language The desired output format: 'latex', 'html', or other for plain text.
#' @return A string formatted in APA style with the desired output format.
#' @examples
#' results <- data.frame(
#'   concept = c("InteractionContextvoluntary", "AnotherConcept"),
#'   feature = c("median", "median"),
#'   difference = c(0.5, 0.7),
#'   t_value = c(2.5, 3.1),
#'   df = c(30, 28),
#'   p_value = c(0.015, 0.002),
#'   lwr = c(0.2, 0.3),
#'   upr = c(0.8, 1.1)
#' )
#' feature_ttest_apa(results, "InteractionContextvoluntary", "median", "latex")
#'
#' @importFrom dplyr filter select pull %>%
#'
#' @export

feature_ttest_apa <- function(results_df, concept_in, feature_in, language) {

  # Extract and format various statistics from the results data frame
  diff <- results_df %>%
    filter(concept == concept_in, feature == feature_in) %>%
    select(difference) %>%
    pull %>%
    round(., 2) %>%
    format(., nsmall=2)
  t <- results_df %>%
    filter(concept == concept_in, feature == feature_in) %>%
    select(t_value) %>%
    pull %>%
    round(., 2) %>%
    format(., nsmall=2)
  df <- results_df %>%
    filter(concept == concept_in, feature == feature_in) %>%
    select(df) %>%
    pull %>%
    round(., 2) %>%
    format(., nsmall=2)
  p <- results_df %>%
    filter(concept == concept_in, feature == feature_in) %>%
    select(p_value) %>%
    pull

  # Format p-value
  p <- ifelse(p < 0.001, "< .001",
              paste0("= ", format(round(p, 3), nsmall=3)))
  lwr <- results_df %>%
    filter(concept == concept_in, feature == feature_in) %>%
    select(lwr) %>%
    pull %>%
    round(., 2) %>%
    format(., nsmall=2)
  upr <- results_df %>%
    filter(concept == concept_in, feature == feature_in) %>%
    select(upr) %>%
    pull %>%
    round(., 2) %>%
    format(., nsmall=2)

  # Format output based on the desired language (LaTeX, HTML, or plain text)
  if (language == "latex") {
    out <- paste("\\textit{difference} = ", diff,
                 ", $t$(", df, ") = ", t,
                 ", $p$ ", p ,
                 ", \\textit{95\\%CI} [", lwr,
                 ", ", upr, "]",
                 sep = "")
  } else if (language == "html") {
    out <- paste("<i>difference</i> = ", diff,
                 ", <i>t</i>(", df, ") = ", t,
                 ", <i>p</i> ", p ,
                 ", <i>95%CI</i> [", lwr,
                 ", ", upr, "]",
                 sep = "")
  } else {
    out <- paste("difference = ", diff,
                 ", t(", df, ") = ", t,
                 ", p ", p ,
                 ", 95%CI [", lwr,
                 ", ", upr, "]",
                 sep = "")
  }

  return(out)
}
