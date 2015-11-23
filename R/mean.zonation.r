#'Calculate mean proportion remaining
#'
#'Calculate the mean proportion of features remaining when a given proportion of landscape is protected.
#'
#'@param x a zonation object.
#'@param p numeric proportion of landscape protected.
#'@param method the method of calculating the mean arithmetic or geometric.
#'@param ... further arguments passed to or from other methods.
#'
#'@export

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

  method %<>% base::match.arg();

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
