#' Run the program zonation from R
#'
#' Run the zonation software for conservation planning on a set of files or raster objects
#' and return the output as a list.
#'
#' @param features RasterStack or file paths of the raster files for features in the conservation plan.
#' @param params a matrix of feature parameter values. Matrix must have 5 columns and one row for each feature. If unset all values default to 1.See Zonation manual for details.
#' @param settings a named list of settings equivalent to the zonation settings file (see zonation manual for details and list of settings)
#' @param alpha numeric uncertainty parameter.
#' @param dist_smooth logical. should distribution smoothing be used.
#' @param kernel_width_mult numeric. factor to multiply feature dispersal kernel widths by.
#' @param dir a directory to house tmp files.
#' @param command_args character string of command line arguments. See zonation manual for details.
#' @param additional_settings additional settings.
#'
#' @importFrom raster brick raster stack writeRaster
#' @importFrom rgdal gdalDrivers
#' @importFrom methods setGeneric setMethod
#' @importFrom utils read.table
#' @importFrom tools file_path_sans_ext
#'
#' @examples
#' library(raster)
#' r1 <- raster(matrix(runif(200^2, 0, 1), 200))
#' r2 <- raster(matrix(runif(200^2, 0, 1), 200))
#' plan <- zonation(stack(r1, r2))
#'
#' @export

methods::setGeneric(
  "zonation",
  function(
    features, params = NULL, settings = NULL, dir = NULL, alpha = 0,
    dist_smooth = FALSE, kernel_width_mult = 1, command_args = NULL,
    additional_settings = NULL
  ) {
    standardGeneric("zonation")
  }
)

zonation_raster <- function(features, params, settings, dir, alpha, dist_smooth,
                            kernel_width_mult, command_args,
                            additional_settings) {

  if (is.null(dir)){
    feature_dir <- tempfile("")
    dir.create(feature_dir)
    dir_remove <- TRUE
  } else {
    feature_dir <- dir
    dir_remove <- FALSE
  }

  rand_fname <- tempfile("feature", feature_dir)

  feature_files <- paste0(rand_fname, ".tif")

  raster::writeRaster(
    x         = features,
    file      = feature_files,
    overwrite = TRUE,
    bylayer   = TRUE,
    suffix    = "names"
  )

  feature_files <- paste0(
    dirname(feature_files), "/", basename(rand_fname), "_", names(features),
    ".tif"
  )

  plan <- zonation(
    features = feature_files, params, settings, dir, alpha, dist_smooth,
    kernel_width_mult, command_args, additional_settings
  )

  if (dir_remove) unlink(feature_dir, TRUE)

  plan

}

#' @describeIn zonation run the program zonation for a RasterStack
methods::setMethod("zonation", c(features = "RasterStack"), zonation_raster)

#' @describeIn zonation run the program zonation for a RasterStack
methods::setMethod("zonation", c(features = "RasterBrick"), zonation_raster)

#' @describeIn zonation run the program zonation for raster files
methods::setMethod(
  "zonation",
  c(features = "character"),
  function(
    features, params, settings, dir, alpha, dist_smooth, kernel_width_mult,
    command_args, additional_settings
  ) {
    zp <- getOption("rzonation.path")
    if (!nzchar(zp)) stop("zonation binary not found")

    if (is.null(dir)) {
      dir <- tempfile("")
      dir.create(dir)
      dir_remove <- TRUE
    } else {
      dir_remove <- FALSE
    }

    datfile <- tempfile(tmpdir = dir)

    if (is.null(settings)) settings <- list()

    default_settings <- c(
      "removal_rule" = 1, "warp factor" = 1000, "edge removal" = 1,
      "add edge points" = 0, "annotate name" = 0
    )

    for (i in names(default_settings))
      if (is.null(settings[[i]])) settings[[i]] <- default_settings[[i]]

    settings <- paste0(
      "[Settings]\n",
      paste(
        names(settings),
        format(settings, scientific = FALSE),
        sep = " = ",
        collapse = "\n"
      ), "\n"
    )

    cat(settings, file = datfile)

    if (is.null(additional_settings)) additional_settings <- list()

    for (i in seq_along(additional_settings)) {
      additional_setting <- paste(
        names(additional_settings[[i]]),
        format(additional_settings[[i]], scientific = FALSE),
        sep = " = ",
        collapse = "\n"
      )
      additional_setting <- paste0(
        "[", names(additional_settings[i]), "]\n", additional_setting, "\n\n"
      )
      cat(additional_setting, file = datfile, append = TRUE)
    }

    spfile <- tempfile(tmpdir = dir)

    nfeatures <- length(features)

    if (!is.null(params)) {
      if (
        !all(
          is.matrix(params),
          ncol(params) == 5,
          nrow(params) == nfeatures,
          is.numeric(params)
        )
      ) {
        stop("params must be a 5 * nfeatures matrix")
      }

      params <- data.frame(params)
      params <- do.call(paste, params)
      params <- paste(params, features, collapse = '\n')

    } else {

      params <- paste0("1 1 1 1 1 ", features, "\n", collapse = "")

    }

    cat(params, file = spfile)

    resstem <- tempfile(tmpdir = dir)

    if (is.null(command_args)) command_args <- "--use-threads=1"

    zig_args <- c(
      "-r", datfile, spfile, resstem, alpha, as.numeric(dist_smooth),
      kernel_width_mult, 1, command_args
    )

    zig_out <-
      try(
        system2(
          getOption("rzonation.path"),
          zig_args,
          stdout = TRUE,
          stderr = TRUE
        )
      )

    if (class(zig_out) == "try-error") return (NA_real_)

    features_info_file <- paste0(resstem, ".features_info.txt")

    features_info <- scan(
      features_info_file, skip = 2, what = "char", quiet = TRUE
    )

    features_info <- matrix(features_info, nrow = nfeatures, byrow = TRUE)
    features_info <- as.data.frame(features_info, stringsAsFactors = FALSE)
    names(features_info) <- c(
      "weight",
      "dist_sum",
      "ig_retain",
      "t_viol_fract_rem",
      "dist_mean_x",
      "dist_mean_y",
      "map_file_name"
    )

    for (i in 1:6) {
      features_info[[i]] <- as.numeric(features_info[[i]])
    }

    curves_file <- paste0(resstem, ".curves.txt")

    curves <- utils::read.table(
      curves_file,
      col.names = c(
        "prop_landscape_lost",
        "cost_need_for_top_frac",
        "min_prop_rem",
        "ave_prop_rem",
        "w_prop_rem",
        "ext_1",
        "ext_2",
        paste0(
          "prop_",
          tools::file_path_sans_ext(features),
          "_rem"
        )
      ),
      skip = 1L
    )

    raster_files <- basename(path = resstem)
    raster_files <- list.files(
      path = dir, pattern = raster_files, full.names = TRUE
    )
    raster_files <- grep(pattern = "\\.tif$", x = raster_files, value = TRUE)

    rasters <- raster::brick(
      raster::stack(
        raster::raster(raster_files[1]), raster::raster(raster_files[2])
      )
    )

    names(rasters) <- c("rank", "wrscr")

    run_info_file <- paste0(resstem, ".run_info.txt")

    run_info <- readLines(run_info_file)

    plan <- list(
      features_info = features_info,
      curves        = curves,
      rasters       = rasters,
      run_info      = run_info
    )

    plan <- structure(plan, class = "zonation")

    if (dir_remove) unlink(dir, TRUE)

    plan

  }

)
