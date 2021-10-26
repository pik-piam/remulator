#' Calculate points of fitted curve for plotting using fit coefficients
#' 
#' This function uses the raw data on which you performed the fit (\code{\link{calculate_fit}}) and the resulting 
#' fit coefficients to calculate a couple of points on the fitted curve that can be used to plot the curve (\code{\link{plot_curve}}).
#' 
#' @param data_in MAgPIE object containing the same data that was used to calculate the fitcoefficients.
#' @param fitcoef MAgPIE object containing the fitcoefficients (output of \code{\link{calculate_fit}})
#' @param myform Function that was fitted and is used here to calculate curve
#' @param ylimit Choose the method to calculate the upper limit for the y values. Options: "individual", the maximal value up to which 
#' the y values of the supplycurve are calculated is done individualy for each reagion and year. "common" (default), the upper limit of  
#' the y values of the supplycurve for all regions and years is the same and is the maximum of the raw data. This is useful for plotting,
#' because it expands "short" supplycurves beyond the maximal demand of the respective region and year and makes them better comparable.
#' @return MAgPIE object containing the points on the curve.
#' @author David Klein
#' @seealso \code{\link{calculate_fit}} \code{\link{plot_curve}}
#' @importFrom magclass unwrap as.magpie getSets setNames collapseNames add_dimension mbind getSets<- ndata clean_magpie
#' @export

calc_supplycurve <- function(data_in,fitcoef,myform,ylimit="common") {
  
  ##########################################################################
  ##### D A T A: Calculate supplycurces (for plotting) #####################
  ##########################################################################
  
  # If data has global dimension but it is not the only one then remove it before searching for global maximum
  if (!setequal(getRegions(data_in),"GLO") & "GLO" %in% getRegions(data_in)) data_in <- data_in["GLO",,,invert=TRUE]
  
  scenario_names <- getItems(data_in, dim="scenario")

  # convert MAgPIE object into matrix
  data_in <- unwrap(data_in)
  
  if (ylimit == "individual") {
    data <- data_in[,,,,,drop=FALSE] # do not select a particular scenario -> max across all scenarios
    # Find greatest raw data in each year and region.
    prod_max <- apply(data,c(1,2,5),max,na.rm=TRUE)
    # silly back and forth conversion to drop only variable but not scenario. Keeping it  
    # an array would drop both because both dimensions have only one element.
    prod_max <- unwrap(collapseNames(as.magpie(prod_max)[,,"x"],collapsedim = "variable"))
    prod_max[is.infinite(prod_max)] <- NA
    prod_max <- as.magpie(prod_max)
  }
  
  res <- NULL
  
  
  for (scen in scenario_names) {

    # ###### find min/max of raw data for calculating supplycurve within this regional range ######
    # limits <- apply(data,c(1,2,3,5),range,na.rm=TRUE)
    # 
    # # provide names for min and max
    # dimnames(limits)[[1]] <- c("min","max")
    # 
    # limits <- as.magpie(limits)
    # 
    # # Provide range where no range could be calculated
    # limits[,,"min"][is.infinite(limits[,,"min"])] <- 0
    # limits[,,"max"][is.infinite(limits[,,"max"])] <- 1
    # 
    # getSets(limits) <- c("region","year","limit","scenario","variable")
  
    if (ylimit == "individual") {
      
      # although prod_max has no scenario dimension (since maxima have been determined across scenarios) provide scenario name here
      # to make it work further down (names of prod_max are used for output (pri and dem) and output needs scenario names)
      getNames(prod_max) <- scen

    } else {
  
      data <- data_in[,,scen,,,drop=FALSE] # select a particular scenario -> max for a particular scenario
      
      # set spatial dimension to zero to prevent automatic detection of the spatial dimension by as.magpie, 
      # it would otherwise detect three letter scenario names as spatial dimension
      max_glo <- as.magpie(apply(data,c(3,5),max,na.rm=TRUE),spatial=0,temporal=0) 
  
      # For a nicer plot: expand supplycurves beyond the maximal demand of the respective region and year.
      # This is useful for early years where only very few points at low demand and low prices have been fitted.
      # Therefore, find maximal price (among all regions and years) for each scenario (global upper limit for y-axis).
      # Calculate the demand that results to this maximal price for each region and year ("invert" supply curve)
      prod_max <- apply(unwrap(fitcoef[,,scen]),c(1,2,3),bisect,myform,approx_this =max_glo[,,"y"][,,scen] ,lower =0,upper =max_glo[,,"x"][,,scen] ,eps = 0.01*max_glo[,,"y"][,,scen])
      
      prod_max <- as.magpie(prod_max)
      prod_max_glo <- prod_max + NA # create empty object of the shape of prod_max (with regions and years)
      prod_max_glo[,,] <- collapseNames(max_glo[,,"x"],collapsedim = "variable") # fill all regions and years with global maximum which has only GLO and no year
    
      # max_reg <- as.magpie(apply(data[,,,,"x",drop=FALSE],c(1,3,5),max,na.rm=TRUE))
      # prod_max_reg <- prod_max + NA # create empty object of the shape of prod_max (with regions and years)
      # prod_max_reg[,,] <- collapseNames(max_reg,collapsedim = "variable") # fill all regions and years with global maximum which has only GLO and no year
  
      # take minimum of maximal real production and maximal inverted production 
      # (so that flat supplycurves are not expanded to the inverted maximum which would prolong the x-axis)
      prod_max <- pmin(prod_max,prod_max_glo)
  
    }
  
    # # If no limit could be calculated (NA), replace it with the maximum of the remaining years of this region
    # prod_max_help <- apply(prod_max,1,max,na.rm=TRUE) # find maximum among years for each region
    # for (r in dimnames(prod_max)$region) {
    #   prod_max[r,,][is.na(prod_max[r,,])] <- prod_max_help[r]
    # }
    
    #prod_max <- fill_missing_years(as.magpie(prod_max),nofit=as.magpie(is.na(prod_max)))
  
    ###### As basis for supplycurve: create demand points between 0 and maximal demand sample 
    # First, create a magpie object with n steps between 0 and 1 in the third dimension.
    # The third dimension is named x01 ... x41
    # The resulting magpie object has the dimensions ["GLO",NULL,x01...x41]
    scale <- setNames(as.magpie(seq(0,1.0,length.out = 41)),gsub(" ","0",paste0("s",format(1:41))))
    # Then multiply with max demand
    dem <- prod_max * scale
    
    # calculate supplycurves using user defined function 'myform'
    # Why use [[]] syntax in the user defined function and why making a list out of fit coefficients?
    # To make the user defined function applicable for two different cases: for a scalar multiplication (in 'calculate_fit')
    # and for a magpie object multiplication in this function
    # The user defined function is used at two places:
    # 1. in calculate_fit, where there variable 'param' is a vector, i.e. each element contains a single number (fit coefficient). There is no 
    #    region or year dimension because it is called via apply
    # 2. here, where each element of 'param' contains a full magpie object with regions, years, and scenario so that it can be 
    #    multiplied with 'dem', which has the same dimensions. Each fit coefficient goes into one of the list elements.
    # The [[]] syntax allows to either select single vector elements in case 1) or single list elements in case 2) with full 
    # magpie objects behind each list element
    param <- list()
    for(i in 1:ndata(fitcoef[,,scen])) param[[i]] <- fitcoef[,,scen][,,i]
    pri <- myform(param,dem) # old way: #pri <- (fitcoef[,,"a"] + (dem^fitcoef[,,"d"]) * fitcoef[,,"c"])
    # pri has messy names due to the multiplication with param. Bring it to the same structure as dem
    getNames(pri) <- getNames(dem) # take names from dem (has two dimension less than pri)
    pri <- clean_magpie(pri)       # remove obsolete dimensions
    #getSets(pri) <- getSets(dem)   # rename dimensions
    
    dem <- add_dimension(dem,dim=3.3, add="type",nm="x")
    pri <- add_dimension(pri,dim=3.3, add="type",nm="y")
    tmp <- mbind(dem,pri)
    getSets(tmp) <- c("region","year","scenario","step","type")
    res <- mbind(res,tmp)
  }

  return(res)
}