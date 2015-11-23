#' Calculate mean proportion remaining
#'
#' Calculate the mean proportion of features remaining when a given proportion of landscape is protected.
#'
#' @param x a zonation object.
#' @param p numeric proportion of landscape protected.
#' @param method the method of calculating the mean arithmetic or geometric.
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' library(raster)
#' r1 <- raster(matrix(rnorm(100, 10)))
#' r2 <- raster(matrix(rnorm(100, 10)))
#' plan <- zonation(stack(r1, r2))
#' mean(plan)
#'
#' @export

mean.zonation <-
  function(x, p = .1, method = c("arithmetic", "geometric"), ...) {
  if (!identical(length(p), 1L) || !base::is.numeric(p)) {
    base::stop("p must be scalar");
  }
  if (p == 0) return(0);
  if (p == 1) return(1);
  if (p < 0 || p > 1) base::stop("p must be a proportion");

  row <-
    x$curves$prop_landscape_lost %>%
    magrittr::subtract(1 - p) %>%
    base::abs() %>%
    base::which.min() %>%
    magrittr::extract(x$curves, ., );

  base::match.arg(method) %>%
  base::switch(
    arithmetic =
      row["ave_prop_remain"],
    geometric =
      row[-1:-7] %>%
      base::log() %>%
      base::as.numeric() %>%
      base::mean(...) %>%
      base::exp()
  );
}
