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
#' @importFrom raster brick raster readAll stack writeRaster
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
    features, params = NULL, settings = NULL, dir = NULL, alpha = 0, dist_smooth = FALSE,
    kernel_width_mult = 1, command_args = NULL, additional_settings = NULL
  ) {
    standardGeneric("zonation");
  }
);

zonation_raster <-
  function(
    features, params, settings, dir, alpha, dist_smooth, kernel_width_mult,
    command_args, additional_settings
  ) {
  if (base::is.null(dir)){
  feature_dir <- base::tempfile("");
  base::dir.create(feature_dir);
  dir_remove <- TRUE;
  } else {
    feature_dir <- dir;
    dir_remove <- FALSE;
  }

  rand_fname <- base::tempfile("feature", feature_dir);

  feature_files <- base::paste0(rand_fname, ".tif");

  raster::writeRaster(
    x         = features,
    file      = feature_files,
    overwrite = TRUE,
    bylayer   = TRUE,
    suffix    = "names"
  );

  feature_files <-
    base::paste0(
      base::dirname(feature_files),
      "/",
      base::basename(rand_fname),
      "_",
      base::names(features),
      ".tif"
    );

  plan <-
    zonation(
      features = feature_files,
      params,
      settings,
      dir,
      alpha,
      dist_smooth,
      kernel_width_mult,
      command_args,
      additional_settings
    );

  if (dir_remove) base::unlink(feature_dir, TRUE);

  plan;

};

#' @describeIn zonation run the program zonation for a RasterStack
setMethod(
  "zonation",
  base::c(features = "RasterStack"),
  zonation_raster
);

#' @describeIn zonation run the program zonation for a RasterStack
setMethod(
  "zonation",
  base::c(features = "RasterBrick"),
  zonation_raster
);

#' @describeIn zonation run the program zonation for raster files
setMethod(
  "zonation",
  base::c(features = "character"),
  function(
    features, params, settings, dir, alpha, dist_smooth, kernel_width_mult,
    command_args, additional_settings
  ) {
    zp <- base::getOption("rzonation.path");
    if (!base::nzchar(zp)) base::stop("zonation binary not found");

    if (base::is.null(dir)){
      dir <- base::tempfile("");
      base::dir.create(dir);
      dir_remove <- TRUE;
    } else {
      dir_remove <- FALSE;
    }

    datfile <- base::tempfile(tmpdir = dir);

    if (base::is.null(settings)) {
      settings <- base::list();
    };

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
        base::format(settings, scientific = FALSE),
        sep = " = ",
        collapse = '\n'),'\n'
    ) %>%
    base::cat(file = datfile);

    if (base::is.null(additional_settings)) {
      additional_settings <- base::list();
    };

    for (i in base::seq_along(additional_settings)) {
      base::paste(
        base::names(additional_settings[[i]]),
        base::format(additional_settings[[i]], scientific = FALSE),
        sep = " = ",
        collapse = "\n"
      ) %>%
      base::paste0("[", base::names(additional_settings[i]), "]\n", ., "\n\n") %>%
      base::cat(file = datfile, append = TRUE);
    };

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
      base::do.call(base::paste, .) %>%
      base::paste(features, collapse = '\n')%>%
      base::cat(file = spfile);
    } else {
      base::paste0("1 1 1 1 1 ", features, "\n", collapse = "")%>%
      base::cat(file = spfile);
    }

    resstem <- base::tempfile(tmpdir = dir);

    if (is.null(command_args)) {
      command_args <- "--use-threads=1"
    };

    zig_args <-
      base::c(
        "-r",
        datfile,
        spfile,
        resstem,
        alpha,
        as.numeric(dist_smooth),
        kernel_width_mult,
        1,
        command_args
      );

    zig_out <-
      base::try(
        base::system2(
          base::getOption("rzonation.path"),
          zig_args,
          stdout = TRUE,
          stderr = TRUE
        )
      );

    if (class(zig_out) == "try-error") return (NA_real_)

    features_info_file <- base::paste0(resstem, ".features_info.txt");

    features_info <-
      features_info_file %>%
      base::scan(skip = 2, what = "char", quiet = TRUE) %>%
      base::matrix(nrow = nfeatures, byrow = TRUE)      %>%
      base::as.data.frame(stringsAsFactors = FALSE)     %>%
      readr::type_convert(.) %>%
      magrittr::set_colnames(.,
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

    curves_file <- base::paste0(resstem, ".curves.txt");

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
      );

    rasters <- brick(stack(raster(raster_files[1]), raster(raster_files[2])))

    names(rasters) <- c("rank", "wrscr")

    run_info_file <- base::paste0(resstem, ".run_info.txt");

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

    if(dir_remove) base::unlink(dir, TRUE);

    plan;

  }

);
