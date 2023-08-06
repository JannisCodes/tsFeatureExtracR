#' Generate plots based on t-test results
#'
#' @param df An annotated dataframe of t-test results
#'
#' @return A list of ggplots
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_linerange geom_vline scale_x_continuous theme
#'
#' @export

generate_feature_plots <- function(df) {
  feature_plot_list <- list()
  for (feature in unique(df$feature)) {
    feature_data <- df[df$feature == feature, ]
    df_plot <- data.frame(label = feature_data$label,
                          difference = feature_data$difference,
                          lwr = feature_data$lwr,
                          upr = feature_data$upr)

    feature_plot_list[[feature]] <- ggplot2::ggplot(df_plot, aes(y = label)) +
      ggplot2::geom_point(aes(x = difference), shape=15, size=2) +
      ggplot2::geom_linerange(aes(xmin = lwr, xmax = upr)) +
      ggplot2::geom_vline(xintercept = 0, linetype="dashed") +
      ggplot2::scale_x_continuous(expand = expansion(mult = 0)) +
      theme_Publication() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.y = element_text(hjust=0),
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank())
  }
  return(feature_plot_list)
}
