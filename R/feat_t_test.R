#' Perform a t-test for a given feature and concept
#'
#' @param df A data frame with the data
#' @param cluster_var The name of the clustering variable in df
#' @param concept The name of the current concept
#' @param ts_feature The name of the current feature
#'
#' @return A t-test result
#' @export

feat_t_test <- function(df, cluster_var, concept, ts_feature) {
  column_name <- paste0(concept, "_", ts_feature)

  group1 <- df[df[[cluster_var]] == unique(df[[cluster_var]])[1], column_name]
  group2 <- df[df[[cluster_var]] == unique(df[[cluster_var]])[2], column_name]

  t.test(group1, group2)
}
