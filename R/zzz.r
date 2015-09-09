# Check for zig4 binary

.onLoad                                                                       <-
  function(libname, pkgname)                                                   {
  op                                                                          <-
    base::options()
  op.rzonation                                                                 <-
    base::list(
      rzonation.path = base::Sys.which('zig4')
    )
  toset                                                                       <-
    op.rzonation                                                             %>%
    base::names(.)                                                           %>%
    magrittr::is_in(
      x     = .,
      table = base::names(op)
    )                                                                        %>%
    magrittr::not(.)                                                           ;

  if (base::any(toset))                                                        {
    base::options(op.rzonation[toset])                                       ;};

  base::invisible()                                                          ;};
