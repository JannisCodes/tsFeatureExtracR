#' Create a custom publication fill scale for ggplot
#'
#' @param ... Other arguments passed on to layer functions
#' @return A discrete fill scale for ggplot with predefined colour palette.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(iris, aes(Species)) + geom_bar(aes(fill = Species)) + scale_fill_Publication()
#' }
#' @importFrom ggplot2 discrete_scale
#' @importFrom scales manual_pal

scale_fill_Publication <- function(...){
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}
