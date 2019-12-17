#' Plot a zonation object
#'
#' Plot a zonation object using ggplot
#'
#' @param data a zonation object.
#' @param ... other arguments.
#'
#' @importFrom ggplot2 aes coord_equal element_blank facet_wrap geom_raster
#' @importFrom ggplot2 ggplot theme theme_bw
#' @importFrom raster getValues ncell xyFromCell
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

ggplot.zonation <- function(data, ...) {
    r <- data[["rasters"]]
    ncells <- raster::ncell(r)
    xy <- raster::xyFromCell(r, seq_len(ncells))
    r <- raster::getValues(r)
    r <- as.data.frame(r)
    r <- utils::stack(r)
    names(r) <- c("value", "variable")
    r <- cbind(r, xy)

    ggplot2::ggplot(r) +
    ggplot2::aes(x = r[["x"]], y = r[["y"]], fill = r[["value"]], ...) +
    ggplot2::geom_raster() +
    ggplot2::coord_equal() +
    ggplot2::facet_wrap(~variable) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text  = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    )
  }
