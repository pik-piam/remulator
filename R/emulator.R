#' Calculate and plot fit.
#' 
#' This function fits a function of the form \code{y = a + b * x ^ d} for the given data.
#' 
#' @param data MAgPIE object containing at least two variables and the modelstatus
#' @param name_x Name of the variable in \code{data} that will be treated as x in the fit
#' @param name_y Name of the variable in \code{data} that will be treated as y in the fit
#' @param name_modelstat Name of the variable that contains the modelstatus
#' @param treat_as_feasible GAMS model status codes that will be regarded feasible. 
#' See \url{https://www.gams.com/24.8/docs/userguides/mccarl/modelstat_tmodstat.htm}
#' @param userfun Function to fit. User can provide a functional form using the following 
#' syntax: \code{function(param,x)return(param[[1]] + param[[2]] * x ^param[[3]])}. This function is the default.
#' @param initial_values Vector with initial values of the fit coefficients.
#' @param outlier_range Before the actual fit a linear pre-fit is performed. Based on their distance to this 
#' pre-fit the data points are allocated to quartiles. A data point is considered an outlier if it is more 
#' than \code{outlier_range} times of the interquartile range away from the the upper or lower end of the 
#' interquartile range \url{http://colingorrie.github.io/outlier-detection.html}.
#' @param n_suff Minial number (default=1) of data points in a specific year and region that 
#' will be regarded as sufficient to perform a fit.
#' If the number of available data points is less no fit will be generated for this year and region.
#' @param fill Logical (default=FALSE) indicating whether data will be copied from subsequent year if in the 
#' current year not enough data points are avaialbe.
#' @param output_path Path to save the output to
#' @param create_pdf Logical indicating whether a pdf should be produced that compiles all figures.
#' @param ... Arguments passed on to the \code{optim} function in \code{calcualte_fit}. Useful to define bounds on fit coefficients.
#' @return MAgPIE object containning fit coefficients
#' @author David Klein
#' @importFrom magclass getSets<- getNames getNames<- add_dimension collapseNames new.magpie
#' @export

emulator <- function(data,name_x,name_y,name_modelstat=NULL,treat_as_feasible=c(2,7),userfun=function(param,x)return(param[[1]] + param[[2]] * x ^param[[3]]),initial_values=c(0,0,1),outlier_range=1.5,n_suff=1,fill=FALSE,output_path="emulator",create_pdf=TRUE,...) {
  
  cat("Starting generation of emulator.\n")
  
  ########################################################
  ################ structure data ########################
  ########################################################

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
    cat("Data has no 'sample' dimension. All scenarios will be treated as one scenario with n samples.\n")
    # replace scenario names with numbers
    getNames(data,dim=1) <- 1:length(getNames(data,dim=1))
    # rename "scenario" dimension to "sample"
    getSets(data) <- gsub("^scenario$","sample",getSets(data))
    # add scenario dimension with only one scenario named "default"
    data <- add_dimension(data,dim=3.1,add="scenario",nm="default")
  } else {
    stop("Input data has to have the following sets: either\n",sets_ready_to_use,"\n or\n",sets_for_single_scenario,"\n but has\n",getSets(data))
  }

  cat("Selecting relevant variables only.\n")
  if (is.null(name_modelstat)) {
    # rename variables to generic short names
    data <- data[,,c(name_x,name_y)]
    getNames(data,dim="variable") <- c("x","y")
  } else {
    # append modelstat if it exists
    # pick variables as provided by user
    if(!name_modelstat %in% getNames(data, dim="variable")) stop("Could not find any variable with the name ",name_modelstat," you provided in name_modelstat in your data!")
    data <- data[,,c(name_x,name_y,name_modelstat)]
    # rename variables to generic short names
    getNames(data,dim="variable") <- c("x","y","modelstat")
  }

  # remove model dimension
  if ("model" %in% getSets(data)) {
    if (length(getNames(data,dim="model"))>1) stop("Data must contain only ONE model not ",length(getNames(data,dim="model")))
    data <- collapseNames(data,collapsedim = "model")
  }
  
  ########################################################
  ################# filter raw data ######################
  ########################################################
  
  # Filter data before fitting: set infeasible, duplicated, and outlier data to NA
  
  # save raw data before filtering for plotting later
  raw <- data
  
  # set data in infeasible years and in subsequent years to NA
  if (!is.null(name_modelstat)) {
    cat("Removing data of infeasible years.\n")
    data <- mute_infes(data, name="modelstat", feasible = treat_as_feasible)
    # remove modelstat because it is not needed anymore and allows proceeding from 
    # here on the same way regardless of whether modelstat existed before or not
    data <- data[,,"modelstat",invert=TRUE]
    raw  <-  raw[,,"modelstat",invert=TRUE]
    
  } else {
    attr(data,"infeasible_flag")  <- NA
    # Attach infes_count as attribute to data
    attr(data,"infeasible_count") <- NA
    # Attach data points that are considered infeasible
    attr(data,"infeasible_data")  <- NA
  }

  # get magpie object marking infeasible years (only for plotting below)
  infes <- attributes(data)$infeasible_flag
  
  # set duplicated samples to NA
  cat("Removing duplicates.\n")
  data <- mute_duplicated(data)

  # Find outliers and set them to NA
  cat("Removing outliers.\n")
  data <- mute_outliers(data,range=outlier_range)
  
  # Set insufficient data points to NA
  cat("Checking if number of remaining data points is sufficient (>=",n_suff,").\n")
  data <- mute_insufficient(data,n_suff)
  
  # save data before fill_missing_years for plottting
  data_before_fillling <- data

  # If in current year not enough data is availalbe (TRUE in nodata) copy it from other years
  if(fill) {
    cat("Copy data to years with unsufficient number of data points.\n")
    nodata <- attributes(data)$insufficient_flag
    data <- fill_missing_years(data,nodata)
  }
  
  ########################################################
  ########### calculate fit coefficients #################
  ########################################################
  
  cat("Calculating fit coefficients.\n")
  fitcoef <- calculate_fit(data["GLO",,,invert=TRUE],form =userfun,initial_values = initial_values,...)
  
  # attach information "takenfrom" (originally created by fill_missing_years) to fitcoef (since it is the return value of this function)
  if (fill) attr(fitcoef,"inputtakenfrom") <- attr(data,"takenfrom")
  
  # # fill missing years
  # cat("Fill missing years.\n")
  # fitcoef <- fill_missing_years(fitcoef,nofit=(collapseNames(fitcoef[,,"b"],collapsedim="coeff"))==0)

  ########################################################
  ######### calculate supplycurve for plotting ###########
  ########################################################
  
  cat("Calculating supplycurve.\n")
  supplycurve_commonY <- calc_supplycurve(data,fitcoef,myform=userfun)
  supplycurve_indiviY <- calc_supplycurve(data,fitcoef,myform=userfun,ylimit="individual")
  
  ########################################################
  ######## label data points for scatter plots ###########
  ########################################################
  
  # Gather all information about filtered data (infeasible, duplicates,
  # outliers) in one MAgPIE object that will be used for plotting.
  cat("Gathering data for plotting.\n")
  
  # separate raw data into different columns of filtered
  filtered <- add_dimension(raw, dim=3.4,add="type",nm="raw")
  filtered <- add_columns(filtered,dim=3.4,addnm = c("fitted","duplicated","infeasible","outliers","insufficient","copied"))
  filtered[,,"fitted"]       <- data_before_fillling
  filtered[,,"duplicated"]   <- attributes(data)$duplicated_data
  filtered[,,"infeasible"]   <- attributes(data)$infeasible_data
  filtered[,,"outliers"]     <- attributes(data)$outliers_data
  filtered[,,"insufficient"] <- attributes(data)$insufficient_data
  if (!is.null(attributes(data)$copied_data)) filtered[,,"copied"] <- attributes(data)$copied_data
  
  ########################################################
  #################### save data #########################
  ########################################################
  
  for (scen in getNames(filtered,dim="scenario")) {
    path_data <- file.path(output_path,scen)
    ifelse(!dir.exists(path_data), dir.create(path_data), FALSE)
    f <- file.path(path_data,paste0("data_postfit_",scen,".Rdata"))
    cat("Saving data to",f,"\n")
    save(data,filtered,fitcoef,userfun,file = f)
  }
  
  ########################################################
  ########### plot supplycurves (png/pdf) ################
  ########################################################
  
  cat("Plotting supplycurve.\n")
  plot_curve(filtered[,,"raw",invert=TRUE],supplycurve_commonY,supplycurve_indiviY,infes,output_path,create_pdf)
  
  return(fitcoef)
}