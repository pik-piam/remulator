#' Replace fits in years where they are flat with fits from other years
#'
#' This function considers fits with a slope less than defined in threshold flat and tries to
#' replace them with non-flat fits from other years.
#'
#' @param path_to_postfit_rdata Path to the Rdata file that contains raw data,
#' fitted data and fitcoefficients saved by the emulator function
#' @param flat If you want this function to replace further fits provide
#' their region and year here using a vector of the form
#' c("LAM:2020,2025", "IND:2005")
#' @param threshold Defines the value of the slope (b) below which fits are considered flat
#' @param plot logical. If TRUE supply curves will be saved to png files
#' @author David Klein
#' @seealso \code{\link{emulator}} \code{\link{fill_missing_years}}
#' @importFrom magclass getNames write.magpie
#' @export

replace_flat_fits <- function(path_to_postfit_rdata, flat = NULL, threshold = 0.01, plot = FALSE) {

  data     <- NULL
  filtered <- NULL
  userfun  <- NULL

  # create subfolder "replaced-flat" within the original data folder
  path_output <- file.path(dirname(path_to_postfit_rdata), "replaced-flat")
  ifelse(!dir.exists(path_output), dir.create(path_output), FALSE)

  # extract fitname (expecting that fitname is the name of the directory where the data file is located in)
  fitname <- basename(dirname(path_to_postfit_rdata))

  cat("Loading", path_to_postfit_rdata, "\n")
  obj <- load(path_to_postfit_rdata, verbose = TRUE)
  
  # Before setting fits flat in the years provided by the user find fits among the original fits that are flat 
  flatByThreshold <- fitcoef[,,"b"] < threshold
  
  # create empty magpie object of the shape of fitcoef[,,"b"]
  flatByUser <- new.magpie(getRegions(fitcoef),
                           getYears(fitcoef),
                           names = getNames(fitcoef[,,"b"]),
                           fill = FALSE,
                           sets = getSets(fitcoef))
  # if provided by the user replace fit coefficients according to users choices
  # expects vector of strings like "LAM:2010,2020"
  if (!is.null(flat)) {
    cat("The following fits were specified by the user and are considered flat in addition to those automatically detected:\n")
    # separate region from years
    flat <- strsplit(flat, ":", fixed = TRUE)
    for (line in flat) {
      region <- line[1]
      # separate years
      year <- as.numeric(unlist(strsplit(line[2], ",", fixed = TRUE)))
      #cat(region, ":", year, "\n")
      # set fit flat so it will be replaced
      flatByUser[region, year, ] <- TRUE
    }
  }
  cat("\nFits listed as flat by the user:\n")
  print(flatByUser)

  # Inform user about flat fits that were only automaticly detected and not defined by user
  onlyFlatByThreshold <- flatByThreshold & !flatByUser
  tmp <- fitcoef[,,"b"]
  tmp[!onlyFlatByThreshold] <- NA
  cat("\nFits detected as flat and NOT listed by the user:\n")
  print(tmp)
  
  # flat fits
  zero <- flatByThreshold | flatByUser

  # Find regions that have only flat fits and set their fitcoefficients to NA for all years.
  # All NAs will be replaced by MOINPUT with artificial fits (= high prices)
  all_flat <- apply(zero, 1, all)
  all_flat[is.na(all_flat)] <- FALSE # if no fit available
  reg_all_flat <- names(all_flat)[all_flat]
  if (length(reg_all_flat) > 0) {
    cat("No non-flat fit available for", reg_all_flat, ". Setting fitcoefficients to NA.\n")
    fitcoef[reg_all_flat,,] <- NA
  }

  # replace fitcoefficients for years that have flat fits (b==0)
  cat("\nReplacing flat fits.\n")
  fitcoef <- fill_missing_years(fitcoef, zero)

  # Calculate number of fitcoefficients that have been replaced
  n <- sum(!is.na(attributes(fitcoef)$takenfrom))
  cat("Number of fits that are considered flat and have been replaced:", n, "\n")

  cat("The following table shows, for each region and year, for a fit that was classified as flat, the year by whose data it was replaced.\n")
  print(attributes(fitcoef)$takenfrom)

  # Save data to files
  scen <- getNames(fitcoef, dim = "scenario")

  if (n > 0) {
    regionscode <- ifelse(is.null(attributes(data)$regionscode), "", paste0("_", attributes(data)$regionscode))

    f <- file.path(path_output, paste0("f30_bioen_price_", scen, "_replaced_flat", regionscode, ".cs4r"))
    cat("Writing fit coefficients to textfile", f, ".\n")
    write.magpie(fitcoef, file_name = f)

    f <- file.path(path_output, paste0("data_postfit_", scen, "_replaced_flat.Rdata"))
    cat("Saving data to", f, "\n")
    save(list = (obj), file = f)

    if (plot) {
      # Calculate and plot supplycurves
      cat("Calculating supplycurves.\n")
      supplycurve_commonY <- calc_supplycurve(data, fitcoef, myform = userfun)
      supplycurve_indiviY <- calc_supplycurve(data, fitcoef, myform = userfun, ylimit = "individual")

      plot_curve(filtered[,,"raw", invert = TRUE], supplycurve_commonY, supplycurve_indiviY, infes = NA, emu_path = ".", fitname = file.path(fitname, "replaced-flat"), create_pdf = FALSE)
    }

    logfile <- file.path(path_output, paste0("replace-flat-fits-", scen, ".log"))

  } else {

    cat("Replaced nothing. Stopping here.\n")
    logfile <- file.path(path_output, paste0("nothing-replaced-", scen, ".log"))

  }

  sink(logfile)
  cat("Number of fits that are considered flat and have been replaced:", n, "\n")
  print(attributes(fitcoef)$takenfrom)
  sink()

}
