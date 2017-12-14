#' Calculate points of fitted curve for plotting using fit coefficients
#' 
#' This function uses the raw data on which you performed the fit (\code{\link{calculate_fit}}) and the resulting 
#' fit coefficients to calculate a couple of points on the fitted curve that can be used to plot the curve (\code{\link{plot_curve}}).
#' 
#' @param data MAgPIE object containing the same data that was used to calculate the fitcoefficients.
#' @param fitcoef MAgPIE object containing the fitcoefficients (output of \code{\link{calculate_fit}})
#' @return MAgPIE object containing the points on the curve.
#' @author David Klein
#' @seealso \code{\link{calculate_fit}} \code{\link{plot_curve}}
#' @importFrom magclass unwrap as.magpie getSets setNames collapseNames add_dimension mbind getSets<-

calc_supplycurve <- function(data,fitcoef) {
  
  ##########################################################################
  ##### D A T A: Calculate supplycurces (for plotting) #####################
  ##########################################################################
  
  # convert MAgPIE object into matrix
  data <- unwrap(data)
  
  ###### find min/max of raw data for calculating supplycurve within this regional range ######
  limits <- apply(data,c(1,2,3,5),range,na.rm=TRUE)
  
  # provide names for min and max
  dimnames(limits)[[1]] <- c("min","max")
  
  limits <- as.magpie(limits)
  
  # Provide range where no range could be calculated
  limits[,,"min"][is.infinite(limits[,,"min"])] <- 0
  limits[,,"max"][is.infinite(limits[,,"max"])] <- 1
  
  getSets(limits) <- c("region","year","limit","scenario","variable")
  
  # For a nicer plot: expand supplycurves beyond the maximal demand of the respective region and year.
  # This is useful for early years where only very few points at low demand and low prices have been fitted.
  # Therefore, find maximal price (among all regions and years) for each scenario (global upper limit for y-axis)
  max_glo <- as.magpie(apply(data,c(3,5),max,na.rm=TRUE))
  
  # calculate the demand that results to this maximal price for each region and year (invert supply curve)
  prod_max_inverted <- ((max_glo[,,"y"] - fitcoef[,,"a"]) / fitcoef[,,"b"])^(1/fitcoef[,,"d"])
  prod_max_inverted <- collapseNames(prod_max_inverted,collapsedim = c(2,3,4,5)) # keep the scenario even if it's only one
  getSets(prod_max_inverted) <- c("region","year","scenario")
  
  # find global maximum of all production
  tmp <- collapseNames(max_glo[,,"x"],collapsedim = 2) # has only GLO and no year
  prod_max_glo_real <- prod_max_inverted + NA # create empty object of the shape of prod_max_inverted (with regions and years)
  prod_max_glo_real[,,] <- tmp # fill regions and years with global maximum
  
  # take minimum of maximal real production and maximal inverted production 
  # (so that flat supplycurves are not expanded to the inverded maximum which would prolong the x-axis)
  prod_max <- pmin(prod_max_inverted,prod_max_glo_real)
  
  ###### As basis for supplycurve: create demand points between 0 and maximal demand sample 
  # First, create a magpie object with 41 steps between 0 and 2 in the third dimension.
  # The third dimension is named x01 ... x41
  # The resulting magpie object has the dimensions ["GLO",NULL,x01...x41]
  scale <- setNames(as.magpie(seq(0,1.0,length.out = 41)),gsub(" ","0",paste0("s",format(1:41))))
  
  # Then multiply with max demand
  #dem <- collapseNames(limits[,,"max"][,,"production"] * scale)
  #dem <- collapseNames(prod_max * scale)
  dem <- prod_max * scale
  
  # calculate supplycurves
  pri <- (fitcoef[,,"a"] + (dem^fitcoef[,,"d"]) * fitcoef[,,"b"])
  pri <- collapseNames(pri,collapsedim = c(1,4,5)) # keep scenario even if it' only one
  
  dem <- add_dimension(dem,dim=3.3, add="type",nm="x")
  pri <- add_dimension(pri,dim=3.3, add="type",nm="y")
  res <- mbind(dem,pri)
  getSets(res) <- c("region","year","scenario","step","type")
  return(res)
}