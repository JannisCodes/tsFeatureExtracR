#' Calculate Column Percentage of Missing Values
#'
#' This function calculates the percentage of missing values in a given vector or data frame column.
#'
#' @param x A vector or data frame column in which to calculate the percentage of missing values.
#'
#' @return A numeric value representing the percentage of missing values in the input vector or column.
#'
#' @examples
#' \dontrun{
#' column <- c(1, 2, NA)
#' column_missing_percentage <- pColMiss(column)
#' }
#' @export

pColMiss <- function(x) {
  sum(is.na(x)) / length(x) * 100
}
