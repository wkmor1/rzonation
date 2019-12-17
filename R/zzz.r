# Check for zig4 binary

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.rzonation <- list(
    rzonation.path =
      if (.Platform$OS.type == "windows") {
        Sys.which("zig4.exe")
      } else {
        Sys.which("zig4")
      }
  )
  toset <- !names(op.rzonation) %in% names(op)

  if (any(toset)) options(op.rzonation[toset])

  invisible()
}

