#' Run t-tests for all provided features and concepts
#'
#' @param df A data frame with the data
#' @param cluster_var The name of the clustering variable in df
#' @param ts_features A vector of features
#' @param concepts A vector of concepts
#'
#' @return A list containing t-test results
#'
#' @export

run_all_tests <- function(df, cluster_var, ts_features, concepts) {
  results <- list()
  for (feature in ts_features) {
    for (concept in concepts) {
      results[[feature]][[concept]] <- feat_t_test(df, cluster_var, concept, feature)
    }
  }
  return(results)
}
