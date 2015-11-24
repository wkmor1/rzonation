#' Plot a zonation object
#'
#' Plot a zonation object using ggplot
#'
#' @param data a zonation object.
#' @param ...other arguments.
#'
#'@importFrom ggplot2 ggplot
#'
#' @examples
#' library(raster)
#' r1 <- raster(matrix(runif(200^2, 0, 1), 200))
#' r2 <- raster(matrix(runif(200^2, 0, 1), 200))
#' plan <- zonation(stack(r1, r2))
#' library(ggplot2)
#' ggplot(plan)
#'
#' @export

ggplot.zonation <-
  function(data, ...) {
    data$rasters %>%
    raster::getValues() %>%
    base::as.data.frame() %>%
    utils::stack() %>%
    magrittr::set_names(
      base::c('value', 'variable')
    ) %>%
    base::cbind(
      data$rasters %>%
      raster::ncell() %>%
      base::seq_len() %>%
      raster::xyFromCell(data$rasters, .)
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = x, y = y), ...
    ) +
    ggplot2::theme_bw() +
    ggplot2::geom_raster(
      ggplot2::aes(fill = value)
    ) +
    ggplot2::coord_equal() +
    ggplot2::facet_wrap( ~variable) +
    ggplot2::theme(
      axis.text  = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    )
  }
