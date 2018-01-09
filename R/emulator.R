#' Calculate and plot fit.
#' 
#' This function fits a function of the form \code{y = a + b * x ^ d} for the given data.
#' 
#' @param data MAgPIE object containing at least two variables and the modelstatus
#' @param name_x Name of the variable in \code{data} that will be treated as x in the fit
#' @param name_y Name of the variable in \code{data} that will be treated as y in the fit
#' @param name_modelstat Name of the variable that contains the modelstatus
#' @param treat_as_infes GAMS model status codes that will be regarded infeasible. See \url{https://www.gams.com/24.8/docs/userguides/mccarl/modelstat_tmodstat.htm}
#' @param output_path Path to save the output to
#' @param create_pdf Logical indicating whether a pdf should be produced that compiles all figures.
#' @param ... Arguments passed on to the \code{optim} function in \code{calcualte_fit}. Useful to define bounds on fit coefficients.
#' @return MAgPIE object containning fit coefficients
#' @author David Klein
#' @importFrom magclass getSets<- getNames getNames<- add_dimension collapseNames
#' @export

emulator <- function(data,name_x,name_y,name_modelstat,treat_as_infes=5,output_path,create_pdf=TRUE,...) {
  
  # name_x="Primary Energy|Biomass|Energy Crops (EJ/yr)"
  # name_y="Price|Primary Energy|Biomass (US$2005/GJ)"
  # name_modelstat="Modelstatus (-)"
  # treat_as_infes = c(5,7)
  # output_path = "single_case"
  # lower=c(0,0,1)
  ########################################################################################################
  ################################ C A L C U L A T E   E M U L A T O R ###################################
  ########################################################################################################

  # if data contains "sample" dimension (n samples for each scenario) -> ok
  # if data does not contain "sample" dimension -> all n scenarios will be lumped together to one scenario with n samples 
  # (with n being the number of previous scenarios)
  
  # The final object before passed on to emulator has to be of the following structure (unsing the set names!). 
  # It may contain a model dimension (with only ONE model) which will be removed
  
  # Formal class 'magpie' [package "magclass"] with 1 slot
  # ..@ .Data: num [1:11, 1:11, 1:42] 0.228 NA NA NA 6.436 ...
  # .. ..- attr(*, "dimnames")=List of 3
  # .. .. ..$ region                  : chr [1:11] "AFR" "CPA" "EUR" "FSU" ...
  # .. .. ..$ year                    : chr [1:11] "y2005" "y2015" "y2025" "y2035" ...
  # .. .. ..$ scenario.sample.variable: chr [1:42] "CDL_base-base.59.x" "CDL_base-base.6.x" "CDL_base-base.60.x" "CDL_base-base.64.x" ...  
  
  sets_ready_to_use <- c("region","year","scenario","sample","model","variable")
  sets_for_single_scenario <- c("region","year","scenario","model","variable")
  if (setequal(getSets(data),sets_ready_to_use)) {
    # Data has required structure, already containing "sample" dimension (n samples for each scenario) -> ok
  } else if (setequal(getSets(data),sets_for_single_scenario)) {
    # Data has no "sample" dimension -> all scenarios will be lumped together to one scenario with n samples
    # replace scenario names with numbers
    getNames(data,dim=1) <- 1:length(getNames(data,dim=1))
    # rename "scenario" dimension to "sample"
    getSets(data) <- gsub("^scenario$","sample",getSets(data))
    # add scenario dimension with only one scenario named "default"
    data <- add_dimension(data,dim=3.1,add="scenario",nm="default")
  } else {
    stop("Input data has to have the following sets: either\n",sets_ready_to_use,"\n or\n",sets_for_single_scenario,"\n but has\n",getSets(data))
  }

  vars <- c(name_x,name_y,name_modelstat)
  names(vars) <- c("x","y","modelstat")
  
  # pick variables as defined in "vars"
  data <- data[,,vars]
  
  # rename variables to names given in "vars"
  getNames(data,dim="variable") <- names(vars)
  
  # remove model dimension
  if ("model" %in% getSets(data)) {
    if (length(getNames(data,dim="model"))>1) stop("Data must contain only ONE model not ",length(getNames(data,dim="model")))
    data <- collapseNames(data,collapsedim = "model")
  }
  
  # set data to NA in infeasible years and the years after
  data <- mute_infes(data = data, name="modelstat", infeasible = treat_as_infes)
  
  # get magpie object marking infeasible years (only for plotting below)
  infes <- mute_infes(data = data, name="modelstat", infeasible = treat_as_infes, return_infes = TRUE)
  
  # calculate fit coefficients
  myfun <- function(param,x)return(param[[1]] + param[[2]] * x ^param[[3]])
  cat("Calculating fit.\n")
  fitcoef <- calculate_fit(data["GLO",,"modelstat",invert=TRUE],form =myfun,initial_values = c(0,0,1),...)
  
  # fill missing years
  cat("Fill missing years.\n")
  fitcoef <- fill_missing_years(fitcoef)

  # calculate supplycurve for plotting
  cat("Calculating supplycurve.\n")
  supplycurve <- calc_supplycurve(data,fitcoef,myform=myfun)
  
  # plot supplycurves to single png files and to pdf
  cat("Plotting supplycurve.\n")
  plot_curve(data,supplycurve,infes,output_path,create_pdf)
  
  return(fitcoef)
  
}

