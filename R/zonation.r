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
#' @param command_args character string of command line arguments. See zonation manual for details.
#'
#' @importFrom raster readAll stack writeRaster
#' @importFrom readr read_file read_table type_convert
#' @importFrom methods setGeneric setMethod
#' @importFrom rgdal gdalDrivers
#'
#' @examples
#' library(raster)
#' r1 <- raster(matrix(runif(200^2, 0, 1), 200))
#' r2 <- raster(matrix(runif(200^2, 0, 1), 200))
#' plan <- zonation(stack(r1, r2))
#'
#' @export

setGeneric(
  "zonation",
  function(
    features, params = NULL, settings = NULL, alpha = 0, dist_smooth = FALSE,
    kernel_width_mult = 1, command_args = NULL
  ) {
    standardGeneric("zonation")
  }
);

#' @describeIn zonation run the program zonation for a RasterStack
setMethod(
   "zonation",
   base::c(features = "RasterStack"),
   function(
     features, params, settings, alpha, dist_smooth, kernel_width_mult,
     command_args
   ) {

     rand_fname <-
       base::tempfile("feature");

     raster::writeRaster(
       x         = features,
       file      =
                   base::paste0(
                     rand_fname,
                     ".tif"
                   ),
       overwrite = TRUE,
       bylayer   = TRUE,
       suffix    = "names"
     );

     features <-
       base::paste0(
         base::tempdir(),
         "/",
         base::basename(rand_fname),
         "_",
         base::names(features),
         ".tif"
       )

     plan <-
       rzonation::zonation(
         features = features
       );

     base::file.remove(features)

     plan
   }
);

#' @describeIn zonation run the program zonation for raster files
setMethod(
  "zonation",
  base::c(features = "character"),
  function(
    features, params, settings, alpha, dist_smooth, kernel_width_mult,
    command_args
  ) {
    zp <- base::getOption("rzonation.path");
    if (!base::nzchar(zp)) base::stop("zonation binary not found");
    dir <- base::tempdir();
    datfile <- base::tempfile(tmpdir = dir);

    if (base::is.null(settings)) {
      settings <- base::list();
    }

    settings %<>%
      bind_if_not_in("removal rule", 1) %>%
      bind_if_not_in("warp factor", 1000) %>%
      bind_if_not_in("edge removal", 1) %>%
      bind_if_not_in("add edge points", 0) %>%
      bind_if_not_in("annotate name", 0);

    base::paste0(
      "[Settings]\n",
      base::paste(
        base::names(settings),
        settings,
        sep = " = ",
        collapse = '\n'
      )
    ) %>%
    base::cat(file = datfile);

    spfile <- base::tempfile(tmpdir = dir);

    nfeatures <- base::length(features);

    if (!is.null(params)) {
      if (
        !all(
          is.matrix(params),
          ncol(params) == 5,
          nrow(params) == nfeatures,
          is.numeric(params)
        )
      ) {
        stop("params must be a 5 * nfeatures matrix");
      }
      base::data.frame(params) %>%
      do.call(paste, .) %>%
      paste(features, collapse = '\n');
    } else {
      base::paste0("1 1 1 1 1 ", features, "\n", collapse = "");
    } %>%
    base::cat(file = spfile);

    resstem <- base::tempfile(tmpdir = dir);

    if (is.null(command_args)) command_args <- "--use-threads=1";

    base::paste(
      base::getOption("rzonation.path"),
      "-r",
      datfile,
      spfile,
      resstem,
      alpha,
      as.numeric(dist_smooth),
      kernel_width_mult,
      1,
      command_args
    ) %>%
    base::system(ignore.stdout = TRUE);

    features_info_file <- base::paste0(resstem, ".features_info.txt")

    features_info <-
      features_info_file %>%
      base::scan(skip = 2, what = "char", quiet = TRUE) %>%
      base::matrix(nrow = nfeatures, byrow = TRUE)      %>%
      base::as.data.frame(stringsAsFactors = FALSE)     %>%
      readr::type_convert(.) %>%
      magrittr::set_colnames(
        base::c(
          "weight",
          "dist_sum",
          "ig_retain",
          "t_viol_fract_rem",
          "dist_mean_x",
          "dist_mean_y",
          "map_file_name"
        )
      );

    curves_file <- base::paste0(resstem, ".curves.txt")

    curves <-
      readr::read_table(
        file      = curves_file,
        col_names = base::c(
                      "prop_landscape_lost",
                      "cost_need_for_top_frac",
                      "min_prop_rem",
                      "ave_prop_rem",
                      "w_prop_rem",
                      "ext_1",
                      "ext_2",
                      base::paste0(
                        "prop_",
                        tools::file_path_sans_ext(x = features),
                        "_rem"
                      )
                    ),
        col_types = {
                    "d"                   %>%
                    base::rep(nfeatures)  %>%
                    base::c("diddddd", .) %>%
                    base::paste0(collapse = "");
                  },
        skip      = 1
      );

    raster_files <-
      base::basename(path = resstem) %>%
      base::list.files(
        path = dir,
        pattern = .,
        full.names = TRUE
      ) %>%
      base::grep(
        pattern = "\\.tif$",
        x = .,
        value = TRUE
      )

    rasters <-
      raster::stack(x = raster_files) %>%
      magrittr::set_names(
        value = base::c("rank", "wrscr")
      ) %>%
      raster::readAll(object = .);

    run_info_file <- base::paste0(resstem, ".run_info.txt")

    run_info <-
      readr::read_file(file = run_info_file);

    plan <-
      base::list(
        features_info = features_info,
        curves        = curves,
        rasters       = rasters,
        run_info      = run_info
      ) %>%
      base::structure(class = "zonation");

    base::file.remove(
      base::c(
        datfile,
        spfile,
        features_info_file,
        curves_file,
        raster_files,
        run_info_file
      )
    )

    plan

  }

);
