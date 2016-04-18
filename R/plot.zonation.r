#' Plot a zonation object
#'
#' Plot a zonation object using ggplot
#'
#' @param obj a zonation object.
#' @param features a numeric vector of columns to call from curves data.frame.
#' @param features a character vector of names that correlate to biodiversity feature names (this is to make pretty plots).
#' @param blackwhite FALSE make plot black and white.
#' @param ... other arguments.
#'
#'@importFrom ggplot2 ggplot
#'
#' @examples
#' library(raster)
#' r1 <- raster(matrix(runif(200^2, 0, 1), 200))
#' r2 <- raster(matrix(runif(200^2, 0, 1), 200))
#' plan <- rzonation::zonation(stack(r1, r2))
#' plot(plan)
#'
#' @export

plot.zonation <-
  function(obj, features=NULL, feature.names=NULL,
           main='Performance curves',
           legend.title='Performance',
           blackwhite=FALSE, ...) {

    curves <- obj$curves
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
       curves[,base::c("prop_landscape_lost",base::colnames(curves)[features])] %>%
       reshape2::melt(id.vars='prop_landscape_lost',
                      measure.vars=base::colnames(.)[-1])%>%
       ggplot2::ggplot(ggplot2::aes_(x=~prop_landscape_lost, y=~value, colour=~variable),...)+
       ggplot2::geom_line()+
       ggplot2::scale_colour_manual(name=legend.title,
                          values=colours)+
       ggplot2::ggtitle(main)+
       ggplot2::theme_bw() +
       ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank())
  }
