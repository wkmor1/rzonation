#'Run the program zonation from R
#'
#'Runs the zonation software for conservation planning on a set of GeoTiff files
#'and returns the output as a list.
#'
#'@param features RasterStack or file paths of the raster files for features in the conservation plan.
#'
#'@importFrom raster readAll stack writeRaster
#'@importFrom readr read_file read_table type_convert
#'@importFrom methods setGeneric setMethod
#'
#'@export

setGeneric(
  "zonation",
  function(features) {
    standardGeneric("zonation")
  }
);

#' @describeIn zonation run the program zonation for a RasterStack
setMethod(
   "zonation",
   base::c(features = "RasterStack"),
   function(features) {
     rand_fname <-
       base::tempfile("feature")
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

     rzonation::zonation(
       features = base::paste0(
         base::tempdir(),
         "/",
         base::basename(rand_fname),
         "_",
         base::names(features),
         ".tif"
       )
     );
   }
);

#' @describeIn zonation run the program zonation for raster files
setMethod(
  "zonation",
  base::c(features = "character"),
  function(features) {
    zp <- base::getOption("rzonation.path");
    if (!base::nzchar(zp)) base::stop("zonation binary not found");
    dir <- base::tempdir();
    datfile <- base::tempfile(tmpdir = dir);

    base::paste(
      "[Settings]",
      "removal rule = 1",
      "warp factor = 1000",
      "edge removal = 1",
      "add edge points = 0",
      "annotate name = 0",
      sep = "\n"
    ) %>%
    base::cat(file = datfile);

    spfile <- base::tempfile(tmpdir = dir);

    base::paste0("1 1 1 1 1 ", features, "\n", collapse = "") %>%
    base::cat(file = spfile);

    resstem <- base::tempfile(tmpdir = dir);

    base::paste(
      base::getOption("rzonation.path"),
      "-r",
      datfile,
      spfile,
      resstem,
      "0.0 0 1.0 1"
    ) %>%
    base::system(ignore.stdout = TRUE);

    nfeatures <- base::length(features);

    features_info <-
      base::paste0(resstem, ".features_info.txt")       %>%
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

    curves <-
      readr::read_table(
        file      = base::paste0(resstem, ".curves.txt"),
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

    rasters <-
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
      ) %>%
      raster::stack(x = .) %>%
      magrittr::set_names(
        value = base::c("rank", "wrscr")
      ) %>%
      raster::readAll(object = .);

    run_info <-
      readr::read_file(
        file = base::paste0(resstem, ".run_info.txt")
      );

    base::list(
      features_info = features_info,
      curves        = curves,
      rasters       = rasters,
      run_info      = run_info
    ) %>%
    base::structure(class = "zonation")
  }
);

utils::globalVariables("weight")
