#' Plot a zonation object
#'
#' Plot a zonation object using ggplot
#'
#' @param x a zonation object.
#' @param features a numeric vector of columns to call from curves data.frame.
#' @param feature.names character vector of feature names to select for plotting
#' @param invert logical invert x-axis.
#' @param main title of plot, default is 'Performance curves'.
#' @param legend.title title of legend, default is 'Performance'.
#' @param blackwhite FALSE make plot black and white.
#' @param ... other plot arguments.
#'
#'@importFrom ggplot2 ggplot
#'
#' @examples
#' library(raster)
#' r1 <- raster(matrix(runif(200^2, 0, 1), 200))
#' r2 <- raster(matrix(runif(200^2, 0, 1), 200))
#' plan <- rzonation::zonation(stack(r1, r2))
#' plot_curves(plan)
#'
#' @export

plot_curves <-
  function(x, features=NULL, feature.names=NULL,invert=TRUE,
           main='Performance curves',
           legend.title='Performance',
           blackwhite=FALSE, ...) {

    curves <- x$curves
    if(invert) {
      curves$prop_landscape_lost <- 1- curves$prop_landscape_lost
      yl <- "Prop. of distributions protected"
    } else { yl <- "Prop. of distributions remaining"}
    header <- base::c("prop_landscape_lost",
                "cost_need_for_top_frac",
                "min_prop_rem",
                "ave_prop_rem",
                "w_prop_rem",
                "ext_1",
                "ext_2")
    if(base::is.null(features)){ features <- base::c(base::length(header)+1):base::ncol(curves)
    } else { features = features
    }
    if(base::is.null(feature.names)){ header <- base::c(header, paste("f", 1:(base::ncol(curves) - base::length(header)), sep=""))
    } else { header <- base::c(header, feature.names)
    }
    if (blackwhite) {
      colours <- grDevices::grey.colors(n=base::length(base::unique(features)))
    } else {
      colours <- base::suppressWarnings(RColorBrewer::brewer.pal(base::length(base::unique(features)),
                                                           "Set1"))
    }

       base::colnames(curves) <- header
       if (base::length(features)>9){
         cat('Too many features, plotting the mean\n')
         dat <- curves[,base::c("prop_landscape_lost","ave_prop_rem",base::colnames(curves)[features])] %>%
                reshape2::melt(measure.vars=base::colnames(.)[-1:-2])
         labs <- base::c("ave_prop_rem","features","features")
         dat %>%
           ggplot2::ggplot(ggplot2::aes_(x=~prop_landscape_lost, y=~value, group=~variable))+
           ggplot2::geom_line(colour="#00000025")+
           ggplot2::geom_line(ggplot2::aes_(x=~prop_landscape_lost, y=~ave_prop_rem),colour='red',size=1.5)+
           ggplot2::ggtitle(main)+
           ggplot2::theme_bw() +
           ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                          panel.grid.minor = ggplot2::element_blank())+
           ggplot2::xlab("Prop. of extent under conservation") +
           ggplot2::ylab("Prop. of distributions remaining")
       } else {
         dat <- curves[,base::c("prop_landscape_lost",base::colnames(curves)[features])] %>%
           reshape2::melt(measure.vars=base::colnames(.)[-1])
       dat %>%
       ggplot2::ggplot(ggplot2::aes_(x=~prop_landscape_lost, y=~value, colour=~variable))+
       ggplot2::geom_line()+
       ggplot2::scale_colour_manual(name=legend.title,
                          values=colours)+
       ggplot2::ggtitle(main)+
       ggplot2::theme_bw() +
       ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank())+
         ggplot2::xlab("Prop. of extent under conservation") +
         ggplot2::ylab("Prop. of distributions remaining")
      }
}
