#' Annotate a dataframe of t-test results with asterisks to indicate significance
#'
#' @param df A dataframe of t-test results
#' @param variableLab A dataframe for variable labels
#' @param join_by a vector or string indicating the id variables to merge the labels
#'
#' @return An annotated dataframe
#'
#' @importFrom dplyr mutate left_join %>%
#'
#' @export

annotate_significance <- function(df, variableLab, join_by) {
  df <- df %>%
    dplyr::mutate(star = ifelse(p_value < 0.001, "***", ifelse(p_value < 0.01, "**", ifelse(p_value < 0.05, "*", "")))) %>%
    dplyr::left_join(variableLab, by = join_by)
  return(df)
}
