#' Set data of infeasible years and their successors to NA
#' 
#' This function sets data of infeasible years and all years after the first infeasible
#' year to NA because results after an infeasible year are not meaningful. The modelstatus
#' has to be provided as one of the variables in the data. This function will look for the
#' modelstatus variable with the name given in \code{name}. If \code{return_infes} is TRUE
#' this function return the matrix with infeasible years instead of the data. 
#' 
#' @param data MAgPIE object containing the results of a MAgPIE run and the modelstatus.
#' @param name String providing the name of the variable that holds the modelstatus.
#' @param infeasible Integer vector defining which modelstatus will be treated as infeasible.
#' @return Magpie object with either filtered model data or the modelstatus.
#' @author David Klein
#' @importFrom magclass collapseNames as.magpie new.magpie

mute_infes <- function(data,name="Modelstatus (-)",infeasible=5) {
  
  ##########################################################################
  ##### D A T A: set infeasible years and their successors to NA ###########
  ##########################################################################
  
  # mark infesible years of scenarios with TRUE
  infes <- FALSE
  for (i in infeasible) {
    tmp <- data[,,name]==i
    infes <- tmp | infes
  }
  
  # set infeasible years to NA
  infes[infes]  <- NA
  # and feasible years to 0 (so that they don't affect the cumsum below)
  infes[!infes] <- 0
  # use cumsum to set all years after the first infeasible year to NA because results after an infeasible year are not meaningful
  infes <- as.magpie(apply(X = infes, MARGIN = c(1,3), FUN = cumsum))
  # set feasible years from 0 to 1 so that they don't change the results of the multiplication below)
  infes[!is.na(infes)] <- 1
  # multiplying keeps only feasible years and sets infeasible years to NA
  tmp <- data*infes
  
  # convert back to TRUE/FALSE: infeasible = NA -> TRUE. feasible != NA -> FALSE
  infes <- as.magpie(is.na(infes))

  # find number of TRUE elements
  infes_count <- new.magpie(getRegions(data),getYears(data),getNames(data,dim="scenario"))
  infes_count[,,] <- as.magpie(apply(unwrap(infes),c(1,2,3),sum))
  
  infes <- collapseNames(as.magpie(infes), collapsedim = "variable") # keep the scenario even if it's only one
  
  # create empty object
  infeasible_data <- data + NA
  # pick infeasible data points
  infeasible_data[infes] <- data[infes]
  
  # Overwrite raw data with feasible data.
  # Use [,,] to keep attributes of "data" (have been erased in "tmp" by multiplication above).
  data[,,] <- tmp

  # Attach infes (TRUE/FALSE) as attribute to data before TRUE/FALSE are changed to 1/0
  attr(data,"infeasible_flag") <- infes
  # Attach infes_count as attribute to data
  attr(data,"infeasible_count") <- infes_count
  # Attach data points that are considered infeasible
  attr(data,"infeasible_data") <- infeasible_data

  return(data)
}
