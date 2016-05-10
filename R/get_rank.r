#' Extract the rank raster
#'
#' Extract the rank raster from a zonation object
#'
#' @param x a zonation object.
#'
#' @importFrom raster raster
#'
#' @examples
#' library(raster)
#' r1 <- raster(matrix(runif(200^2, 0, 1), 200))
#' r2 <- raster(matrix(runif(200^2, 0, 1), 200))
#' plan <- zonation(stack(r1, r2))
#' plan_rank <- get_rank(plan)
#' plan_rank
#'
#' @export

get_rank <- function(x) {
  UseMethod("get_rank")
}

#' @describeIn get_rank extract the rank raster
#' @export

get_rank.default <- function(x) {
  stop(paste("No method defined for object of class", class(x)))
}

#' @describeIn get_rank extract the rank raster
#' @export

get_rank.zonation <- function(x) {
  raster(x$rasters, 1)
}
