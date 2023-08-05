#' Analysis and Visualization of Missing Values in Features
#'
#' This function analyzes the missing values in a given features data frame,
#' calculates the percentage of missing values, and visualizes these percentages
#' using a histogram. It returns a list containing the overall percentage of missing
#' values, a data frame detailing the percentage of missing values per feature,
#' and the generated plot.
#'
#' @param features A data frame containing features with potential missing values.
#' @param title A string to be used as the title for the histogram.
#'
#' @return A list with three elements:
#'    * miss_overall: The overall percentage of missing values in the features data frame.
#'    * miss_per_feature: A data frame detailing the percentage of missing values per feature.
#'    * plt_miss_per_feature: A ggplot object representing the histogram of missing value percentages.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(a = c(1, 2, NA), b = c(4, NA, NA))
#' missing_values_analysis <- feature_missing(data, "Missing Values Analysis")
#' }
#' @export
#' @importFrom ggplot2 aes ggplot geom_histogram labs

feature_missing <- function(features, title = "Feature-wise Missingness") {
  # Calculate the total percentage of missing values in the feature data frame
  miss_overall <- pMissTot(features)

  # Apply the pColMiss function to each column in the data frame to calculate
  # the percentage of missing values in each feature. The result is a data frame
  miss_per_feature <- apply(features, 2, pColMiss) %>%
    data.frame(feature = names(.), perc_missing = ., row.names = NULL)

  # Format and construct the title for the plot
  title <- paste0(title, "\n(total missing = ", format(round(miss_overall, 2), nsmall = 2), "%)")

  # Generate a histogram of the missing value percentages for each feature.
  # We omit any NA values, and apply a custom ggplot2 theme for the plot
  plt_miss_per_feature <- miss_per_feature %>%
    na.omit %>%
    ggplot(aes(x = perc_missing)) +
    geom_histogram(binwidth = 1, fill = "black") +
    labs(
      x = "Percent Missing",
      title = title
    ) +
    theme_Publication()

  # The function returns a list containing the overall missing value percentage,
  # the missing value percentages per feature, and the plot
  out <- list(
    miss_overall = miss_overall,
    miss_per_feature = miss_per_feature,
    plt_miss_per_feature = plt_miss_per_feature
  )

  return(out)
}
