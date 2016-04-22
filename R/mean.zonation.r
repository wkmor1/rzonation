#' Calculate mean proportion remaining
#'
#' Calculate the mean proportion of features remaining when a given proportion of landscape is protected.
#'
#' @param x a zonation object.
#' @param p numeric proportion of landscape protected.
#' @param features numeric vector of features id number (order features are added to rzonation)
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

mean.zonation <-
  function(x, p = .1, features=NULL, method = c("arithmetic", "geometric"), ...) {
  if (!identical(length(p), 1L) || !base::is.numeric(p)) {
    base::stop("p must be scalar");
  }
  if (p == 0) return(0);
  if (p == 1) return(1);
  if (p < 0 || p > 1) base::stop("p must be a proportion");
  if (is.null(features)){features<-"ave_prop_rem"
  } else {
    if (!base::is.numeric(features))base::stop("feature must be a numeric vector column indexs")
    features <- features + 7
  }
  row <-
    x$curves$prop_landscape_lost %>%
    magrittr::subtract(1 - p) %>%
    base::abs() %>%
    base::which.min() %>%
    magrittr::extract(x$curves, .,);

  base::match.arg(method) %>%
  base::switch(
    arithmetic =
      row[features] %>%
      base::as.numeric(),
    geometric =
      row[-1:-7] %>%
      base::log() %>%
      base::as.numeric() %>%
      (function(x){
        if(features=='ave_prop_rem')
          base::mean(x, na.rm = TRUE)
        else x
        }) %>%
      base::exp()
  );
}
