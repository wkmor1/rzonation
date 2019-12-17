#' Calculate mean proportion remaining
#'
#' Calculate the mean proportion of features remaining when a given proportion of landscape is protected.
#'
#' @param x A zonation object.
#' @param p Numeric. Proportion of landscape protected.
#' @param features Numeric Vector. Feature id numbers (order features are added to rzonation)
#' @param method the method of calculating the mean arithmetic or geometric.
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' library(raster)
#' r1 <- raster(matrix(runif(200^2, 0, 1), 200))
#' r2 <- raster(matrix(runif(200^2, 0, 1), 200))
#' plan <- zonation(stack(r1, r2))
#' mean(plan)
#'
#' @export

mean.zonation <- function(x, p = .1, features = NULL,
                          method = c("arithmetic", "geometric"), ...) {

  if (!identical(length(p), 1L) || !is.numeric(p)) stop("p must be scalar")
  if (p == 0) return(0L)
  if (p == 1) return(1L)
  if (p < 0L || p > 1L) stop("p must be a proportion")
  if (is.null(features)) {
    features <- "ave_prop_rem"
  } else {
    if (!is.numeric(features)) {
      stop("feature must be a numeric vector of column indices")
    }
    features <- features + 7L
  }

  rows <- x[["curves"]][["prop_landscape_lost"]]
  rows <- abs(rows - (1 - p))
  min_row <- x[["curves"]][which.min(rows), ]

  switch(
    match.arg(method),
    arithmetic = as.numeric(min_row[features]),
    geometric  =
      if (features == "ave_prop_rem") {
        exp(mean(log(as.numeric(min_row[-seq.int(1L, 7L)])), na.rm = TRUE))
      } else {
        as.numeric(min_row[-seq.int(1L, 7L)])
      }
  )
}
