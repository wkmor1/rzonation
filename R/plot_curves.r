#' Plot a zonation object
#'
#' Plot a zonation object using ggplot
#'
#' @param x a zonation object.
#' @param features a numeric vector of columns to call from curves data.frame.
#' @param feature_names character vector of feature names to select for plotting
#' @param invert logical invert x-axis.
#' @param main title of plot, default is 'Performance curves'.
#' @param legend.title title of legend, default is 'Performance'.
#' @param blackwhite FALSE make plot black and white.
#' @param ... other plot arguments.
#'
#' @importFrom ggplot2 aes_ element_blank geom_line ggplot ggtitle
#' @importFrom ggplot2 scale_colour_manual theme theme_bw xlab ylab
#' @importFrom grDevices grey.colors
#' @importFrom RColorBrewer brewer.pal
#' @importFrom reshape2 melt

#'
#' @examples
#' library(raster)
#' r1 <- raster(matrix(runif(200^2, 0, 1), 200))
#' r2 <- raster(matrix(runif(200^2, 0, 1), 200))
#' plan <- rzonation::zonation(stack(r1, r2))
#' plot_curves(plan)
#'
#' @export

plot_curves <- function(x, features = NULL, feature_names = NULL, invert = TRUE,
                        main = "Performance curves",
                        legend.title = "Performance",
                        blackwhite = FALSE, ...) {

  curves <- x[["curves"]]

  if (invert) {
    curves[["prop_landscape_lost"]] <- 1L - curves[["prop_landscape_lost"]]
    yl <- "Prop. of distributions protected"
  } else {
    yl <- "Prop. of distributions remaining"
  }

  header <- c("prop_landscape_lost", "cost_need_for_top_frac", "min_prop_rem",
              "ave_prop_rem", "w_prop_rem", "ext_1", "ext_2")

  if (is.null(features)) features <- seq.int(length(header) + 1L, ncol(curves))

  if (is.null(feature_names)) {
    header <- c(
      header, paste("f", seq_len(ncol(curves) - length(header)), sep = "")
    )
  } else {
    header <- c(header, feature_names)
  }

  colnames(curves) <- header

  if (blackwhite) {
    colours <- grDevices::grey.colors(n = length(unique(features)))
  } else {
    colours <- suppressWarnings(
      RColorBrewer::brewer.pal(length(unique(features)), "Set1")
    )
  }

  if (length(features) > 9L) {
    message("Too many features, plotting the mean\n")
    data <- curves[
      , c("prop_landscape_lost", "ave_prop_rem", colnames(curves)[features])
    ]
    data <- reshape2::melt(data, measure.vars = colnames(data)[-1L:-2L])
    labs <- c("ave_prop_rem", "features", "features")
    ggplot2::ggplot(data) +
    ggplot2::aes_(x = ~prop_landscape_lost, y = ~value, group = ~variable) +
    ggplot2::geom_line(colour = "#00000025") +
    ggplot2::geom_line(
      ggplot2::aes_(x = ~prop_landscape_lost, y = ~ave_prop_rem),
      colour = "red", size = 1.5
    ) +
    ggplot2::ggtitle(main) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::xlab("Prop. of extent under conservation") +
    ggplot2::ylab("Prop. of distributions remaining")
  } else {
    data <- curves[, c("prop_landscape_lost", colnames(curves)[features])]
    data <- reshape2::melt(data, measure.vars = colnames(data)[-1L])
    ggplot2::ggplot(data) +
    ggplot2::aes_(x = ~prop_landscape_lost, y = ~value, colour = ~variable) +
    ggplot2::geom_line() +
    ggplot2::scale_colour_manual(name = legend.title, values = colours) +
    ggplot2::ggtitle(main) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::xlab("Prop. of extent under conservation") +
    ggplot2::ylab("Prop. of distributions remaining")
  }
}
