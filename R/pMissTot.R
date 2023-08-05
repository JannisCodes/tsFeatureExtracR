#' Calculate Total Percentage of Missing Values
#'
#' This function calculates the total percentage of missing values in a given data frame.
#'
#' @param x A data frame in which to calculate the percentage of missing values.
#'
#' @return A numeric value representing the total percentage of missing values in the data frame.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(a = c(1, 2, NA), b = c(4, NA, NA))
#' missing_percentage <- pMissTot(data)
#' }
#' @export

pMissTot <- function(x) {
  sum(is.na(x)) / (ncol(x) * nrow(x)) * 100
}
