#' See output of zonation run
#'
#' See the run information produced by zonation
#'
#' @param object a zonation object.
#' @param ... additional arguments affecting the summary produced.
#'
#' @examples
#' library(raster)
#' r1 <- raster(matrix(runif(200^2, 0, 1), 200))
#' r2 <- raster(matrix(runif(200^2, 0, 1), 200))
#' plan <- zonation(stack(r1, r2))
#' library(ggplot2)
#' summary(plan)
#'
#' @export

summary.zonation <- function(object, ...) cat(object$run_info)
