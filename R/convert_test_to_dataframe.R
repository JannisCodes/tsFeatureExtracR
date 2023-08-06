#' Convert a list of t-test results into a dataframe
#'
#' @param results A list of t-test results
#' @param ts_features A vector of features
#' @param concepts A vector of concepts
#'
#' @return A dataframe containing the results
#'
#' @export

convert_test_to_dataframe <- function(results, ts_features, concepts) {
  results_df <- data.frame(concept = character(),
                           feature = character(),
                           difference = numeric(),
                           df = numeric(),
                           t_value = numeric(),
                           p_value = numeric(),
                           lwr = numeric(),
                           upr = numeric(),
                           stringsAsFactors = FALSE)

  for (feature in ts_features) {
    for (concept in concepts) {
      test_result <- results[[feature]][[concept]]
      difference <- test_result$estimate[1] - test_result$estimate[2]
      t_value <- test_result$statistic
      df <- test_result$parameter
      p_value <- test_result$p.value
      lwr <- test_result$conf.int[1]
      upr <- test_result$conf.int[2]

      results_df <- rbind(results_df, data.frame(concept = concept,
                                                 feature = feature,
                                                 difference = difference,
                                                 t_value = t_value,
                                                 df = df,
                                                 p_value = p_value,
                                                 lwr = lwr,
                                                 upr = upr,
                                                 stringsAsFactors = FALSE))
    }
  }
  return(results_df)
}
