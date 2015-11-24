#' @importFrom magrittr %>% %<>%

# to use magrittr shortcut
utils::globalVariables(".")

# magrittr like functions to return something else if condition is not met
return_if_not <- function(x, test, y) {
  if (test) y else x
}

# bind z to x if z not in y
bind_if_not_in <- function(x, z, z_value = base::get(z, base::parent.frame()), y = x) {
  x %>%
    return_if_not(
      y %>%
        magrittr::extract2(z) %>%
        base::is.null(.),
      x %>%
        magrittr::inset2(z, z_value)
    )
}
